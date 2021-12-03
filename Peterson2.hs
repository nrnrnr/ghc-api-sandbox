{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Peterson2
where

import FastDom

import Data.Function
import Data.List (sortBy)
import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Panic

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

class Labeled a where
  labelOf :: a -> Label

instance Labeled Label where
    labelOf = id

instance NonLocal a => Labeled (a C x) where
    labelOf = entryLabel

class (Monoid c) => Code c where
  type CodeExpr c :: *
  codeLabel :: Label -> c
  gotoExit :: c
  goto :: Label -> Int -> c
  continue :: Label -> Int -> c
  ifStart :: CodeExpr c -> Label -> c
  ifElse :: c
  ifEnd :: c
  repeatStart :: c
  repeatEnd :: c
  blockEntry :: Label -> c
  blockExit :: Label -> c

  codeBody :: MyBlock -> c

begin :: (Labeled a, Code c) => a -> c
begin = blockEntry . labelOf

end :: (Labeled a, Code c) => a -> c
end = blockExit . labelOf

-- case 1

        
data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | TerminalFlow

data StackFrame = PendingElse Label Label
                | PendingEndif
                | PendingNode MyBlock

type Stack = [StackFrame]

--branchCode :: Branch -> Code
--branchCode (BlockCode b) = fst (decompose b)
--branchCode (DirectTarget _) = mempty

structure :: forall c node . (node ~ CmmNode, Code c) => GenCmmGraph node -> c
structure g = doNode (blockLabeled (g_entry g)) []
 where

   doNode   :: MyBlock -> Stack -> c
   doBegins :: MyBlock -> [MyBlock] -> Stack -> c
   doBranch :: Label -> Label -> Stack -> c
   doStack  :: Stack -> c

   doNode x stack = codeLabel (entryLabel x) <> doBegins x (mergeDominees x) stack

   doBegins x (y:ys) stack = begin y  <> doBegins x ys (PendingNode y:stack) -- step 1
   doBegins x [] stack = codeLabel xlabel <> codeBody x <> sequel
     where sequel = case flowLeaving x of
                      Unconditional l -> doBranch xlabel l stack
                      Conditional e t f ->
                          ifStart e xlabel <> doBranch xlabel t (PendingElse xlabel f : stack)
                      TerminalFlow -> gotoExit
           xlabel = entryLabel x

   -- case 2
   doBranch from to stack 
     | isBackward from to = continue to (index to stack) <> unimp "loops" -- case 1 step 4
     | isMergeLabel to = goto to (index to stack) -- could be omitted if to on top of stack
                    <> doStack stack
     | otherwise = doNode (blockLabeled to) stack
           
   -- case 3
   doStack (PendingElse c f : stack) = ifElse <> doBranch c f (PendingEndif : stack)
   doStack (PendingEndif : stack) = ifEnd <> doStack stack
   doStack (PendingNode x : stack) = blockExit (entryLabel x) <> doNode x stack
   doStack [] = mempty

   blockLabeled :: Label -> MyBlock

   GMany NothingO blockmap NothingO = g_graph g
   blockLabeled l = fromJust $ mapLookup l blockmap

   rpblocks :: [MyBlock]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   preds :: Label -> [Label] -- reachable predecessors of reachable blocks
   preds = \l -> mapFindWithDefault [] l predmap
       where predmap :: LabelMap [Label]
             predmap = foldEdges (\from to pm -> addToList (from :) to pm) mapEmpty

   isMergeLabel :: Label -> Bool
   isMergeLabel l = setMember l mergeNodes

   isMergeBlock :: MyBlock -> Bool
   isMergeBlock = isMergeLabel . entryLabel                   

   mergeNodes :: LabelSet
   mergeNodes = setFromList [entryLabel n | n <- rpblocks, big (preds (entryLabel n))]
    where big [] = False
          big [x] = False
          big (_ : _ : _) = True

   isHeader :: Label -> Bool
   isHeader = unimp "isHeader"

   mergeDominees :: MyBlock -> [MyBlock]
   mergeDominees x = filter isMergeBlock $ idominees (entryLabel x)

   index label [] = panic "destination label not on stack"
   index label (frame : stack)
       | matches label frame = 0
       | otherwise = 1 + index label stack
     where matches label (PendingNode b) = label == entryLabel b
           matches _ _ = False

   idominees :: Label -> [MyBlock] -- sorted with highest rpnum first
   rpnum :: Label -> RPNum
   (idominees, rpnum) = (idominees, rpnum)
       where (dominators, rpnums) = dominatorMap' g

             addToDominees ds label rpnum =
               case idom label of
                 EntryNode -> ds
                 AllNodes -> panic "AllNodes appears as dominator"
                 NumberedNode { ds_label = dominator } ->
                     mapAlter (addDominee label rpnum) dominator ds

             dominees :: LabelMap Dominees
             dominees = mapFoldlWithKey addToDominees mapEmpty rpnums

             idom :: Label -> DominatorSet -- immediate dominator
             idom lbl = mapFindWithDefault AllNodes lbl dominators

             idominees lbl = map (blockLabeled . fst) $ mapFindWithDefault [] lbl dominees

             addDominee :: Label -> RPNum -> Maybe Dominees -> Maybe Dominees
             addDominee l rpnum Nothing = Just [(l, rpnum)]
             addDominee l rpnum (Just pairs) = Just (insert pairs)
                 where insert [] = [(l, rpnum)]
                       insert ((l', rpnum') : pairs)
                           | rpnum > rpnum' = (l, rpnum) : (l', rpnum') : pairs
                           | otherwise = (l', rpnum') : insert pairs
                                          
             rpnum lbl =
                 mapFindWithDefault (panic "label without reverse postorder number")
                                    lbl rpnums


   isBackward from to = rpnum to < rpnum from

type RPNum = Int


unimp :: String -> a
unimp s = error $ s ++ " not implemented"

flowLeaving :: MyBlock -> ControlFlow e
flowLeaving b = unimp "flowLeaving"



type Dominees = [(Label, RPNum)] -- ugh. should be in `where` clause


addToList :: (IsMap map) => ([a] -> [a]) -> KeyOf map -> map [a] -> map [a]
addToList consx = mapAlter add
    where add Nothing = Just (consx [])
          add (Just xs) = Just (consx xs)
