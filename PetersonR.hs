{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PetersonR
  ( structuredControl
  )
where

import Debug.Trace


import Prelude hiding (succ)

import GHC.Cmm.Dataflow.Dominators

import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

import GHC.Cmm.Switch

import GHC.Platform

import GHC.Utils.Panic
import GHC.Utils.Outputable (Outputable, text, (<+>), ppr
                            , showSDocOneLine, defaultSDocContext
                            , pprWithCommas
                            )

import GHC.Wasm.ControlFlow

type MyBlock = CmmBlock



-- | Abstracts the kind of control flow we understand how to convert.
-- A block can be left unconditionally, conditionally on a predicate
-- of type `e`, or not at all.  "Switch" style control flow is not
-- yet implemented.

data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | Switch { _scrutinee :: e 
                            , _targets :: [Maybe Label] -- from 0
                            , _defaultTarget :: Maybe Label
                            }
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
                | Unreachable             -- ^ Trap for missing case in switch

instance Outputable StackFrame where
    ppr (PendingElse _ tl) = text "else" <+> ppr tl
    ppr (PendingEndif) = text "endif"
    ppr (PendingNode b) = text "node" <+> ppr (entryLabel b)
    ppr (EndLoop l) = text "loop" <+> ppr l
    ppr (Unreachable) = text "unreachable"

-- | Convert a Cmm CFG to structured control flow expressed as
-- a `WasmStmt`.

structuredControl :: forall node e s . (node ~ CmmNode)
                  => Platform  -- ^ needed for offset calculation
                  -> (CmmExpr -> e) -- ^ translator for expressions
                  -> (Block node O O -> s) -- ^ translator for straight-line code
                  -> GenCmmGraph node -- ^ CFG to be translated
                  -> WasmStmt s e
structuredControl platform txExpr txBlock g =
    doBlock (blockLabeled (g_entry g)) []
 where

   -- | `doBlock` basically handles Peterson's case 1: it emits code 
   -- from the block to the nearest merge node that the block dominates.
   -- `doBegins` takes the merge nodes that the block dominates, and it
   -- wraps the immediately preceding code in `begin...end`, so that
   -- control can transfer to the merge node by means of an exit statement.
   -- And `doBranch` implements a control transfer, which may be
   -- implemented by falling through or by a `br` instruction 
   -- created with `exit` or `continue`.

   doBlock  :: MyBlock -> Stack -> WasmStmt s e
   doBegins :: MyBlock -> [MyBlock] -> Stack -> WasmStmt s e
   doBranch :: Label -> Label -> Stack -> WasmStmt s e

   doBlock x stack = doBegins x (dominees x) stack
     where dominees = case lastNode x of CmmSwitch {} -> allDominees
                                         _ -> mergeDominees
     -- case 1 step 2 (done before step 1)
     -- note dominees must be ordered with largest RP number first

   doBegins x (y:ys) stack =
       blockEndingIn y (doBegins x ys (PendingNode y:stack)) <> doBlock y stack
     where blockEndingIn y = wasmLabeled (entryLabel y) WasmBlock
   doBegins x [] stack =
       WasmLabel (Labeled xlabel undefined) <>
       if isHeader xlabel then
           wasmLabeled xlabel WasmLoop (emitTrappedBlock x (EndLoop xlabel : stack))
       else
           emitTrappedBlock x stack

     -- rolls together case 1 step 6, case 2 step 1, case 2 step 3
     where emitTrappedBlock :: MyBlock -> Stack -> WasmStmt s e
           emitTrappedBlock x stack =
               if isSwitchWithoutDefault x then
                   wasmUnlabeled WasmBlock (emitBlock x (Unreachable : stack)) <> WasmUnreachable
               else
                   emitBlock x stack
           emitBlock x stack =
             codeBody x <>
             case flowLeaving platform x of
               Unconditional l -> doBranch xlabel l stack -- case 1 step 6
               Conditional e t f -> -- case 1 step 5
                 wasmLabeled xlabel WasmIf
                      (txExpr e)
                      (doBranch xlabel t (PendingElse xlabel f : stack))
                      (doBranch xlabel f (PendingEndif : stack))
               TerminalFlow -> WasmReturn
                  -- case 1 step 6, case 2 steps 2 and 3
               Switch e targets default' ->
                   trace ("targets: " ++ pprShow targets ++ "\ndefault: " ++ pprShow default') $
                   trace ("successors: " ++ pprShow (successors x)) $
                   WasmBrTable (txExpr e) (map switchIndex targets) (switchIndex default')
            where switchIndex :: Maybe Label -> Labeled Int
                  switchIndex Nothing = wasmUnlabeled id 0 -- immediate exit
                  switchIndex (Just lbl) = wasmLabeled lbl id (index' lbl stack)
                          

           xlabel = entryLabel x

   -- case 2
   doBranch from to stack 
      | isBackward from to = WasmContinue to i
           -- case 1 step 4
      | isMergeLabel to = WasmExit to i
      | otherwise = doBlock (blockLabeled to) stack
     where i = index to stack

   ---- everything else here is utility functions

   blockLabeled :: Label -> MyBlock
   rpnum :: Label -> RPNum -- ^ reverse postorder number of the labeled block
   forwardPreds :: Label -> [Label] -- ^ reachable predecessors of reachable blocks,
                                   -- via forward edges only
   isMergeLabel :: Label -> Bool
   isMergeBlock :: MyBlock -> Bool
   isHeader :: Label -> Bool -- ^ identify loop headers
   mergeDominees :: MyBlock -> [MyBlock]
     -- ^ merge nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the stack first.
   allDominees :: MyBlock -> [MyBlock]
     -- ^ all nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the stack first.
   dominates :: Label -> Label -> Bool
     -- ^ Domination relation (not just immediate domination)

   codeBody :: Block CmmNode C C -> WasmStmt s e
   codeBody (BlockCC _first middle _last) = WasmSlc (txBlock middle)



   blockLabeled l = fromJust $ mapLookup l blockmap
   GMany NothingO blockmap NothingO = g_graph g

   rpblocks :: [MyBlock]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   forwardPreds = \l -> mapFindWithDefault [] l predmap
       where predmap :: LabelMap [Label]
             predmap = foldEdges addForwardEdge mapEmpty
             addForwardEdge from to pm
                 | isBackward from to = pm
                 | otherwise = addToList (from :) to pm

   isMergeLabel l = setMember l mergeNodes
   isMergeBlock = isMergeLabel . entryLabel                   

   mergeNodes :: LabelSet
   mergeNodes =
       setFromList [entryLabel n | n <- rpblocks, big (forwardPreds (entryLabel n))]
    where big [] = False
          big [_] = False
          big (_ : _ : _) = True

   isHeader = \l -> setMember l headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   mergeDominees = filter isMergeBlock . allDominees
   allDominees x = idominees (entryLabel x)

   index' lbl stack =
       if stackHas stack lbl then index lbl stack
       else panic ("destination label " ++ pprShow lbl ++ " not on stack " ++ pprShow (pprWithCommas ppr stack))

   index _ [] = panic "destination label not on stack"
   index label (frame : stack)
       | matches label frame = 0
       | otherwise = 1 + index label stack
     where matches label (PendingNode b) = label == entryLabel b
           matches label (EndLoop l) = label == l
           matches _ _ = False

   idominees :: Label -> [MyBlock] -- sorted with highest rpnum first
   gwd = graphWithDominators g
   rpnum lbl = mapFindWithDefault (panic "label without reverse postorder number")
               lbl (gwd_rpnumbering gwd)
   (idominees, dominates) = (idominees, dominates)
       where addToDominees ds label rpnum =
               case idom label of
                 EntryNode -> ds
                 AllNodes -> panic "AllNodes appears as dominator"
                 NumberedNode { ds_label = dominator } ->
                     addToList (addDominee label rpnum) dominator ds

             dominees :: LabelMap Dominees
             dominees = mapFoldlWithKey addToDominees mapEmpty (gwd_rpnumbering gwd)

             idom :: Label -> DominatorSet -- immediate dominator
             idom lbl = mapFindWithDefault AllNodes lbl (gwd_dominators gwd)

             idominees lbl = map (blockLabeled . fst) $ mapFindWithDefault [] lbl dominees

             addDominee :: Label -> RPNum -> Dominees -> Dominees
             addDominee l rpnum [] = [(l, rpnum)]
             addDominee l rpnum ((l', rpnum') : pairs)
                 | rpnum == rpnum' = pairs -- no duplicates
                 | rpnum > rpnum' = (l, rpnum) : (l', rpnum') : pairs
                 | otherwise = (l', rpnum') : addDominee l rpnum pairs

             dominates lbl blockname = lbl == blockname || dominatorsMember lbl (idom blockname)

   isBackward from to = rpnum to <= rpnum from -- self-edge counts as a backward edge
    -- XXX need to test a graph with a self-edge


flowLeaving :: Platform -> MyBlock -> ControlFlow CmmExpr
flowLeaving platform b =
    case lastNode b of
      CmmBranch l -> Unconditional l
      CmmCondBranch c t f _ -> Conditional c t f
      CmmSwitch e targets ->
          let (offset, target_labels) = switchTargetsToTable targets
              default_label = switchTargetsDefault targets
              scrutinee = smartPlus platform e offset
          in  Switch scrutinee target_labels default_label
          
      CmmCall { cml_cont = Just l } -> Unconditional l
      CmmCall { cml_cont = Nothing } -> TerminalFlow
      CmmForeignCall { succ = l } -> Unconditional l

smartPlus :: Platform -> CmmExpr -> Int -> CmmExpr
smartPlus _ e 0 = e
smartPlus platform e k =
    CmmMachOp (MO_Add width) [e, CmmLit (CmmInt (fromIntegral k) width)]
  where width = cmmExprWidth platform e


isSwitchWithoutDefault :: MyBlock -> Bool
isSwitchWithoutDefault b =
    case lastNode b of CmmSwitch _ targets -> isNothing (switchTargetsDefault targets)
                       _ -> False



type Dominees = [(Label, RPNum)] -- ugh. should be in `where` clause


addToList :: (IsMap map) => ([a] -> [a]) -> KeyOf map -> map [a] -> map [a]
addToList consx = mapAlter add
    where add Nothing = Just (consx [])
          add (Just xs) = Just (consx xs)


pprShow :: Outputable a => a -> String
pprShow a = showSDocOneLine defaultSDocContext (ppr a)

stackHas :: Stack -> Label -> Bool
stackHas frames lbl = any (matches lbl) frames
     where matches label (PendingNode b) = label == entryLabel b
           matches label (EndLoop l) = label == l
           matches _ _ = False

