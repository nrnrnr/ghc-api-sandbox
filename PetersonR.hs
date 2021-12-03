{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PetersonR
where

import Prelude hiding (succ)

import GHC.Cmm.Dataflow.Dominators

import Data.Kind
import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Panic

type MyBlock = CmmBlock

-- | The type of code ("statements") we intend to emit.
-- This might be a sequence of Wasm instructions.
-- Type `CodeExpr c` is the type of expressions that 
-- can appear as a condition in an `if`.

class (Monoid c) => Code c where
  type CodeExpr c :: Type
  codeLabel :: Label -> c

  repeatx :: Label -> c -> c

  ifx :: CodeExpr c -> Label -> c -> c -> c --

  block :: Label -> c -> c  -- ^ put code in block so `goto` can be replace with `exit`

  goto     :: Label -> Int -> c  -- ^ exit; translates as `br k`
  continue :: Label -> Int -> c  -- ^ restart loop; translates as `br k`

  gotoExit :: c -- ^ stop the function (return or tail call)

  codeBody :: MyBlock -> c -- ^ straight-line code



-- | Abstracts the kind of control flow we understand how to convert.
-- A block can be left unconditionally, conditionally on a predicate
-- of type `e`, or not at all.  "Switch" style control flow is not
-- yet implemented.

data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | TerminalFlow


-- | Peterson's stack.  If I can figure out how to make 
-- the code generator recursive, we can replace the stack
-- with a data structure whose only purpose is to track
-- the nexting level for exit statements.

type Stack = [StackFrame]
data StackFrame = PendingElse Label Label -- ^ YT
                | PendingEndif            -- ^ YF
                | PendingNode MyBlock     -- ^ ordinary node
                | EndLoop Label           -- ^ Peterson end node


-- | Convert a Cmm CFG to structured control flow.
-- The resulting code is intended to be peephole optimized.
structure :: forall c node . (node ~ CmmNode, Code c, CodeExpr c ~ CmmExpr)
          => GenCmmGraph node -> c
structure g = doBlock (blockLabeled (g_entry g)) []
 where

   -- | `doBlock` basically handles Peterson's case 1: it emits code 
   -- from the block to the nearest merge node that the block dominates.
   -- `doBegins` takes the merge nodes that the block dominates, and it
   -- wraps the immediately preceding code in `begin...end`, so that
   -- control can transfer to the merge node by means of an exit statement.
   -- And `doBranch` implements a control transfer, which may be
   -- implemented by falling through or by a `br` instruction 
   -- created with `exit` or `continue`.

   doBlock  :: MyBlock -> Stack -> c
   doBegins :: MyBlock -> [MyBlock] -> Stack -> c
   doBranch :: Label -> Label -> Stack -> c

   doBlock x stack = codeLabel (entryLabel x) <> doBegins x (mergeDominees x) stack
     -- case 1 step 2 (done before step 1)
     -- note mergeDominees must be ordered with largest RP number first

   doBegins x (y:ys) stack =
       block (entryLabel y) (doBegins x ys (PendingNode y:stack)) <> doBlock y stack
   doBegins x [] stack =
       codeLabel xlabel <>
       if isHeader xlabel then repeatx xlabel (continue x (EndLoop xlabel : stack))
       else continue x stack

     -- rolls together case 1 step 6, case 2 step 1, case 2 step 3
     where continue x stack =
             codeBody x <>
             case flowLeaving x of
               Unconditional l -> doBranch xlabel l stack -- case 1 step 6
               Conditional e t f -> -- case 1 step 5
                 ifx e xlabel (doBranch xlabel t (PendingElse xlabel f : stack))
                              (doBranch xlabel f (PendingEndif : stack))
               TerminalFlow -> gotoExit
                  -- case 1 step 6, case 2 steps 2 and 3
           xlabel = entryLabel x

   -- case 2
   doBranch from to stack 
     | isBackward from to = continue to (index to stack)
          -- case 1 step 4
     | isMergeLabel to = goto to (index to stack) -- could be omitted if to on top of stack
     | otherwise = doBlock (blockLabeled to) stack
           

   ---- everything else here is utility functions


   blockLabeled :: Label -> MyBlock
   rpnum :: Label -> RPNum -- ^ reverse postorder number of the labeled block
   preds :: Label -> [Label] -- ^ reachable predecessors of reachable blocks
   isMergeLabel :: Label -> Bool
   isMergeBlock :: MyBlock -> Bool
   isHeader :: Label -> Bool -- ^ identify loop headers
   mergeDominees :: MyBlock -> [MyBlock]
     -- ^ merge nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the stack first.
   dominates :: Label -> Label -> Bool
     -- ^ Domination relation (not just immediate domination)



   blockLabeled l = fromJust $ mapLookup l blockmap
   GMany NothingO blockmap NothingO = g_graph g

   rpblocks :: [MyBlock]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   preds = \l -> mapFindWithDefault [] l predmap
       where predmap :: LabelMap [Label]
             predmap = foldEdges (\from to pm -> addToList (from :) to pm) mapEmpty

   isMergeLabel l = setMember l mergeNodes
   isMergeBlock = isMergeLabel . entryLabel                   

   mergeNodes :: LabelSet
   mergeNodes = setFromList [entryLabel n | n <- rpblocks, big (preds (entryLabel n))]
          -- XXX need to filter out backward branches
    where big [] = False
          big [_] = False
          big (_ : _ : _) = True

   isHeader = \l -> setMember l headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   mergeDominees x = filter isMergeBlock $ idominees (entryLabel x)

   index _ [] = panic "destination label not on stack"
   index label (frame : stack)
       | matches label frame = 0
       | otherwise = 1 + index label stack
     where matches label (PendingNode b) = label == entryLabel b
           matches _ _ = False

   idominees :: Label -> [MyBlock] -- sorted with highest rpnum first
   (idominees, rpnum, dominates) = (idominees, rpnum, dominates)
       where (dominators, rpnums) = dominatorMap' g

             addToDominees ds label rpnum =
               case idom label of
                 EntryNode -> ds
                 AllNodes -> panic "AllNodes appears as dominator"
                 NumberedNode { ds_label = dominator } ->
                     addToList (addDominee label rpnum) dominator ds

             dominees :: LabelMap Dominees
             dominees = mapFoldlWithKey addToDominees mapEmpty rpnums

             idom :: Label -> DominatorSet -- immediate dominator
             idom lbl = mapFindWithDefault AllNodes lbl dominators

             idominees lbl = map (blockLabeled . fst) $ mapFindWithDefault [] lbl dominees

             addDominee :: Label -> RPNum -> Dominees -> Dominees
             addDominee l rpnum [] = [(l, rpnum)]
             addDominee l rpnum ((l', rpnum') : pairs)
                 | rpnum > rpnum' = (l, rpnum) : (l', rpnum') : pairs
                 | otherwise = (l', rpnum') : addDominee l rpnum pairs
                                          
             rpnum lbl =
                 mapFindWithDefault (panic "label without reverse postorder number")
                                    lbl rpnums

             dominates lbl blockname = hasLbl (idom blockname)
               where hasLbl AllNodes = False
                     hasLbl EntryNode = False
                     hasLbl (NumberedNode _ l p) = l == lbl || hasLbl p



   isBackward from to = rpnum to < rpnum from


flowLeaving :: MyBlock -> ControlFlow CmmExpr
flowLeaving b =
    case lastNode b of
      CmmBranch l -> Unconditional l
      CmmCondBranch c t f _ -> Conditional c t f
      CmmSwitch { } -> panic "switch not implemented"
      CmmCall { cml_cont = Just l } -> Unconditional l
      CmmCall { cml_cont = Nothing } -> TerminalFlow
      CmmForeignCall { succ = l } -> Unconditional l
      



type Dominees = [(Label, RPNum)] -- ugh. should be in `where` clause


addToList :: (IsMap map) => ([a] -> [a]) -> KeyOf map -> map [a] -> map [a]
addToList consx = mapAlter add
    where add Nothing = Just (consx [])
          add (Just xs) = Just (consx xs)
