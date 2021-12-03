{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Peterson
where

import Data.Function
import Data.List (sortBy)
import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

{-
Every Block maps onto Peterson's structure in this way:

  - The block's entry label becomes a Peterson node X with a single branch.

  - The body of the block becomes the code along the branch.

  - The block's exit Cmm node becomes a Peterson node X' that has the control flow.

Only an X can be a Peterson merge node.

Only an X' can have multiple successors.

Every Peterson branch corresponds to one of two things:

  - A block body
  - An empty branch that does nothing but transfer control to its single successor.

-}

type MyBlock = CmmBlock

class (Monoid c) => Code c where
  type CodeExpr c :: *
  codeLabel :: Label -> c
  gotoExit :: c
  goto :: Label -> c
  ifStart :: CodeExpr c -> c
  ifElse :: c
  ifEnd :: c
  repeatStart :: c
  repeatEnd :: c

  body :: MyBlock -> c


-- decompose :: MyBlock -> (Code, ControlFlow)

data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | TerminalFlow

data Branch = BlockCode MyBlock
            | DirectTarget Label
            | GotoPetersonEnd Label -- reiterate loop
            | GotoTerminal


data PNode = BlockStart MyBlock
           | BlockEnd MyBlock
           | Terminal
           | LoopEnd MyBlock -- Peterson's "end node"

data StackFrame = PendingElse Label
                | PendingEndif
                | PendingNode PNode

type Stack = [StackFrame]

--branchCode :: Branch -> Code
--branchCode (BlockCode b) = fst (decompose b)
--branchCode (DirectTarget _) = mempty

structure :: forall c node . (node ~ CmmNode, Code c) => GenCmmGraph node -> c
structure g = doBranch (DirectTarget (g_entry g)) [PendingNode Terminal]
 where
   -- case 2 from Peterson
   doBranch :: Branch -> Stack -> c
   doBranch (BlockCode b) stack = body b <> doNode (BlockEnd b) stack
   doBranch (GotoTerminal) stack = gotoExit <> doStack stack
   doBranch (GotoPetersonEnd l) stack = goto l <> doStack stack
   doBranch (DirectTarget label) stack =
       if isMerge label then
           goto label <> doStack stack
       else
           doNode (blockLabeled label) stack

   -- case 1 from Peterson
   doNode :: PNode -> Stack -> c
   doNode x stack =
     thelabel x <> next -- step 2
     where stack'' = if isHeader x then LoopEnd x : stack' else stack'
               where stack'  = sortBy (compare `on` rpnum) : stack -- step 1
           thelabel (BlockStart b) =
               codeLabel (entryLabel b) -- if b is a loop header make this a loop!
           thelabel _ = mempty
           next = -- steps 3, 4, 5, 6
             case x of Terminal -> gotoExit -- step 3 (assert stack is empty?)
                       LoopEnd b -> goto (entryLabel b) -- step 4
                       BlockEnd b -> case flowLeaving b of
                           Conditional e t f -> -- step 5
                               ifStart e <>
                               doBranch (DirectTarget t) (PendingElse f : stack'')
                           Unconditional l ->
                               doBranch (DirectTarget l) stack'' - 6
                           TerminalFlow -> 
                               doBranch GotoTerminal stack'' - 6
                       BlockStart b -> doBranch (BlockCode b) stack'' -- step 6 also

   doStack :: Stack -> c
   doStack (PendingElse l : stack) =
       ifElse <> doBranch (DirectTarget l) (PendingEndif : stack)
   doStack (PendingEndif : stack) = ifEnd <> doStack stack
   doStack (PendingNode n : stack) = doNode n stack

   isMerge :: Label -> Bool
   isMerge = unimp "isMerge"

   GMany NothingO blockmap NothingO = g_graph g
   blockLabeled l = fromJust $ mapLookup l blockmap

   targetSuccessor :: MyBlock -> Branch
   targetSuccessor b = case successors b of
                         [l] -> DirectTarget l
                         [] -> GotoTerminal
                         _ : _ : _ -> error "multiple successors with unconditional flow"

   rpblocks :: [MyBlock]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   rpnum :: Label -> Int
   rpnum l = mapFindWithDefault (error "unreachable block") l rpnums
     where rpnums = mapFromList (zip (map entryLabel rpblocks) [1..]) :: LabelMap Int

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   preds :: Label -> [Label] -- reachable predecessors of reachable blocks
   preds l = get l predmap
       where predmap =
                 foldEdges (\from to pm -> mapInsert to (from : get to pm) pm) mapEmpty
             get = mapFindWithDefault []

   mergeNodes :: LabelSet
   mergeNodes = setFromList [entryLabel n | n <- rpblocks, big preds (entryLabel n)]
    where big [] = False
          big [x] = False
          big (_ : _ : _) = True

   isHeader :: Label -> Bool
   isHeader = unimp "isHeader"

unimp :: String -> a
unimp s = error $ s ++ " not implemented"

flowLeaving :: MyBlock -> ControlFlow e
flowLeaving b = unimp "flowLeaving"

