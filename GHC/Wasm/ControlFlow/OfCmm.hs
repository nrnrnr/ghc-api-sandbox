{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Wasm.ControlFlow.OfCmm
  ( structuredControl
  )
where

import Prelude hiding (succ)

import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Dominators
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


-- | The syntactic constructs in which Wasm code may be contained.
data ContainingSyntax
    = BlockFollowedBy Label
    | LoopHeadedBy Label
    | IfThenElse
    | BlockFollowedByTrap

type Context = [ContainingSyntax]

-- | Convert a Cmm CFG to structured control flow expressed as
-- a `WasmStmt`.

structuredControl :: forall e s .
                     Platform  -- ^ needed for offset calculation
                  -> (Label -> CmmExpr -> e) -- ^ translator for expressions
                  -> (Label -> Block CmmNode O O -> s) -- ^ translator for straight-line code
                  -> CmmGraph -- ^ CFG to be translated
                  -> WasmStmt s e
structuredControl platform txExpr txBlock g =
   if hasBlock isSwitchWithoutDefault g then -- see Note [Paranoid flow]
       wasmUnlabeled WasmBlock (doBlock (blockLabeled (g_entry g)) [BlockFollowedByTrap]) <>
       WasmUnreachable
   else
       doBlock (blockLabeled (g_entry g)) []
 where
   -- Tragic fact: To Cmm, a "block" is a basic block, but to Wasm,
   -- a "block" is a structured control-flow construct akin
   -- to Pascal's `begin... end`.

   -- | `doBlock` basically handles Peterson's case 1: it emits code 
   -- from the block to the nearest merge node that the block dominates.
   --
   -- `doExits` takes the merge nodes that the block dominates, which are
   -- essentially the exits from a subgraph headed by the block.
   -- For each merge node, `doExits` wraps the code that immediately
   -- precedes it in `begin...end`, so that a suitable exit statement
   -- (Wasm `br` instruction) transfers control to the merge node.
   --
   -- And `doBranch` implements a control transfer, which may be
   -- implemented by falling through or by a `br` instruction.

   doBlock  :: CmmBlock               -> Context -> WasmStmt s e
   doExits  :: CmmBlock -> [CmmBlock] -> Context -> WasmStmt s e
   doBranch :: Label -> Label         -> Context -> WasmStmt s e

   doBlock x context = doExits x (dominatees x) context
     where dominatees = case lastNode x of CmmSwitch {} -> allDominatees
                                           _ -> mergeDominatees
     -- N.B. Dominatees must be ordered with largest RP number first.
     -- (In Peterson, this is case 1 step 2, which I do before step 1)

   doExits x (y:ys) context =
       wasmLabeled ylabel WasmBlock (doExits x ys (BlockFollowedBy ylabel : context)) <>
       doBlock y context
     where ylabel = entryLabel y
   doExits x [] context =
       WasmLabel (Labeled xlabel undefined) <>
       if isHeader xlabel then
           wasmLabeled xlabel WasmLoop (emitBlockX (LoopHeadedBy xlabel : context))
       else
           emitBlockX context

     -- (In Peterson, emitBlockX combines case 1 step 6, case 2 step 1, case 2 step 3)
     where emitBlockX context =
             codeBody xlabel x <>
             case flowLeaving platform x of
               Unconditional l -> doBranch xlabel l context -- Peterson: case 1 step 6
               Conditional e t f -> -- Peterson: case 1 step 5
                 wasmLabeled xlabel WasmIf
                      (txExpr xlabel e)
                      (doBranch xlabel t (IfThenElse : context))
                      (doBranch xlabel f (IfThenElse : context))
               TerminalFlow -> WasmReturn
                  -- Peterson: case 1 step 6, case 2 steps 2 and 3
               Switch e targets default' ->
                   wasmLabeled xlabel WasmBrTable (txExpr xlabel e)
                                                  (map switchIndex targets)
                                                  (switchIndex default')
            where switchIndex :: Maybe Label -> Labeled Int
                  switchIndex Nothing = wasmUnlabeled id (trapIndex context)
                  switchIndex (Just lbl) = wasmLabeled lbl id (index' lbl context)
                          

           xlabel = entryLabel x

   -- In Peterson, `doBranch` implements case 2 (and part of case 1)
   doBranch from to context 
      | isBackward from to = WasmContinue to i
           -- Peterson: case 1 step 4
      | isMergeLabel to = WasmExit to i
      | otherwise = doBlock (blockLabeled to) context
     where i = index to context

   ---- everything else here is utility functions

   blockLabeled :: Label -> CmmBlock
   rpnum :: Label -> RPNum -- ^ reverse postorder number of the labeled block
   forwardPreds :: Label -> [Label] -- ^ reachable predecessors of reachable blocks,
                                    -- via forward edges only
   isMergeLabel :: Label -> Bool
   isMergeBlock :: CmmBlock -> Bool
   isHeader :: Label -> Bool -- ^ identify loop headers
   mergeDominatees :: CmmBlock -> [CmmBlock]
     -- ^ merge nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the context first.
   allDominatees :: CmmBlock -> [CmmBlock]
     -- ^ all nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the context first.
   dominates :: Label -> Label -> Bool
     -- ^ Domination relation (not just immediate domination)

   -- | Translate straightline code, which is uninterpreted except by `txBlock`.
   codeBody :: Label -> Block CmmNode C C -> WasmStmt s e
   codeBody lbl (BlockCC _first middle _last) = wasmLabeled lbl WasmSlc (txBlock lbl middle)



   blockLabeled l = fromJust $ mapLookup l blockmap
   GMany NothingO blockmap NothingO = g_graph g

   rpblocks :: [CmmBlock]
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

   isMergeLabel l = setMember l mergeBlockLabels
   isMergeBlock = isMergeLabel . entryLabel                   

   mergeBlockLabels :: LabelSet
   -- N.B. A block is a merge node if it is where control flow merges.
   -- That means it is entered by multiple control-flow edges, _except_
   -- back edges don't count.  There must be multiple paths that enter the
   -- block _without_ passing through the block itself.
   mergeBlockLabels =
       setFromList [entryLabel n | n <- rpblocks, big (forwardPreds (entryLabel n))]
    where big [] = False
          big [_] = False
          big (_ : _ : _) = True

   isHeader = \l -> setMember l headers  -- loop headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   mergeDominatees = filter isMergeBlock . allDominatees
   allDominatees x = idominatees (entryLabel x)

   index' lbl context =
       if stackHas context lbl then index lbl context
       else panic ("destination label " ++ pprShow lbl ++
                   " not in context " ++ pprShow (pprWithCommas ppr context))

   index _ [] = panic "destination label not in context"
   index label (frame : context)
       | matches label frame = 0
       | otherwise = 1 + index label context
     where matches label (BlockFollowedBy l) = label == l
           matches label (LoopHeadedBy l) = label == l
           matches _ _ = False

   trapIndex :: Context -> Int
   trapIndex [] = panic "context does not include a trap"
   trapIndex (BlockFollowedByTrap : _) = 0
   trapIndex (_ : context) = 1 + trapIndex context
  

   idominatees :: Label -> [CmmBlock] 
     -- Immediate dominatees, sorted with highest rpnum first
   gwd = graphWithDominators g
   rpnum lbl = mapFindWithDefault (panic "label without reverse postorder number")
               lbl (gwd_rpnumbering gwd)
   (idominatees, dominates) = (idominatees, dominates)
       where addToDominatees ds label rpnum =
               case idom label of
                 EntryNode -> ds
                 AllNodes -> panic "AllNodes appears as dominator"
                 NumberedNode { ds_label = dominator } ->
                     addToList (addDominatee label rpnum) dominator ds

             dominatees :: LabelMap Dominatees
             dominatees = mapFoldlWithKey addToDominatees mapEmpty (gwd_rpnumbering gwd)

             idom :: Label -> DominatorSet -- immediate dominator
             idom lbl = mapFindWithDefault AllNodes lbl (gwd_dominators gwd)

             idominatees lbl =
                 map (blockLabeled . fst) $ mapFindWithDefault [] lbl dominatees

             addDominatee :: Label -> RPNum -> Dominatees -> Dominatees
             addDominatee l rpnum [] = [(l, rpnum)]
             addDominatee l rpnum ((l', rpnum') : pairs)
                 | rpnum == rpnum' = pairs -- no duplicates
                 | rpnum > rpnum' = (l, rpnum) : (l', rpnum') : pairs
                 | otherwise = (l', rpnum') : addDominatee l rpnum pairs

             dominates lbl blockname =
                 lbl == blockname || dominatorsMember lbl (idom blockname)

   isBackward from to = rpnum to <= rpnum from -- self-edge counts as a backward edge

type Dominatees = [(Label, RPNum)] 
  -- ^ List of blocks that are immediately dominated by a block.
  -- (In a just world this definition could go into a `where` clause.)

flowLeaving :: Platform -> CmmBlock -> ControlFlow CmmExpr
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


isSwitchWithoutDefault :: CmmBlock -> Bool
isSwitchWithoutDefault b =
    case lastNode b of CmmSwitch _ targets -> isNothing (switchTargetsDefault targets)
                       _ -> False


--  Note [Paranoid Flow]
--  
--  If it knows all alternatives to a `case` expression, GHC generates a
--  `CmmSwitch` node without a default label.  But the Wasm target
--  requires a default label for *every* switch.  So if the graph
--  being compiled contains a `CmmSwitch` with no default label,
--  we generate an extra block that is followed by an `unreachable`
--  instruction, and every Wasm `br_table` instruction is given
--  that label as its default target.  That's paranoid.
--  
--  If we truly trust GHC that the default will never run,
--  we could avoid ever emitting that extra block.  The `trapIndex`
--  function would need to be altered to return zero always
--  (since a branch of zero is always safe).






addToList :: (IsMap map) => ([a] -> [a]) -> KeyOf map -> map [a] -> map [a]
addToList consx = mapAlter add
    where add Nothing = Just (consx [])
          add (Just xs) = Just (consx xs)

hasBlock :: (Block node C C -> Bool) -> GenCmmGraph node -> Bool
hasBlock p g = mapFoldl (\b block -> b || p block) False blockmap
   where GMany NothingO blockmap NothingO = g_graph g


------------------------------------------------------------------
--- everything below here is for diagnostics in case of panic


pprShow :: Outputable a => a -> String
pprShow a = showSDocOneLine defaultSDocContext (ppr a)


instance Outputable ContainingSyntax where
    ppr (BlockFollowedBy l) = text "node" <+> ppr l
    ppr (LoopHeadedBy l) = text "loop" <+> ppr l
    ppr (IfThenElse) = text "if-then-else"
    ppr (BlockFollowedByTrap) = text "trap"

stackHas :: Context -> Label -> Bool
stackHas frames lbl = any (matches lbl) frames
     where matches label (BlockFollowedBy l) = label == l
           matches label (LoopHeadedBy l) = label == l
           matches _ _ = False

