{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Wasm.ControlFlow.FromCmm
  ( structuredControl
  )
where

import Prelude hiding (succ)

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Tree as Tree

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch

import GHC.Platform

import GHC.Utils.Panic
import GHC.Utils.Outputable (Outputable, text, (<+>), ppr
                            , pprWithCommas
                            , showSDocUnsafe
                            )

import GHC.Wasm.ControlFlow

-- import Debug.Trace

trace :: String -> a -> a
trace _ a = a

showO :: Outputable a => a -> String
showO = showSDocUnsafe . ppr

maximumSwitchSize :: Int
maximumSwitchSize = 500
  -- ^ Size of the largest switch table for which we will attempt a
  -- `br_table` instruction.


-- | Abstracts the kind of control flow we understand how to convert.
-- A block can be left unconditionally, conditionally on a predicate
-- of type `e`, or not at all.  "Switch" style control flow is not
-- yet implemented.

data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | Switch { _scrutinee :: e
                            , _range :: BrTableInterval
                            , _targets :: [Maybe Label] -- from 0
                            , _defaultTarget :: Maybe Label
                            }
                   | TerminalFlow


-- | The syntactic constructs in which Wasm code may be contained.
data ContainingSyntax
    = BlockFollowedBy Label
    | LoopHeadedBy Label
    | IfThenElse (Maybe Label)
    | BlockFollowedByTrap

matchesFrame :: Label -> ContainingSyntax -> Bool
matchesFrame label (BlockFollowedBy l) = label == l
matchesFrame label (LoopHeadedBy l) = label == l
matchesFrame label (IfThenElse (Just l)) = label == l
matchesFrame _ _ = False

data Context = Context { enclosing :: [ContainingSyntax]
                       , fallthrough :: Maybe Label  -- the label can
                                                     -- be reached just by "falling through"
                                                     -- the hole
                       }

instance Outputable Context where
  ppr c | Just l <- fallthrough c =
                    pprWithCommas ppr (enclosing c) <+> text "fallthrough to" <+> ppr l
        | otherwise = pprWithCommas ppr (enclosing c)

emptyContext :: Context
emptyContext = Context [] Nothing

inside :: ContainingSyntax -> Context -> Context
withFallthrough :: Context -> Label -> Context

inside frame c = c { enclosing = frame : enclosing c }
withFallthrough c l = c { fallthrough = Just l }

-- | Convert a Cmm CFG to structured control flow expressed as
-- a `WasmStmt`.

paranoidFlow :: Bool -- always generate default case?
paranoidFlow = True

structuredControl :: forall e s .
                     Platform  -- ^ needed for offset calculation
                  -> (Label -> CmmExpr -> e) -- ^ translator for expressions
                  -> (Label -> Block CmmNode O O -> s) -- ^ translator for straight-line code
                  -> CmmGraph -- ^ CFG to be translated
                  -> WasmControl s e
structuredControl platform txExpr txBlock g =
   if paranoidFlow && hasBlock isSwitchWithoutDefault g then -- see Note [Paranoid flow]
       WasmBlock (doNode dominatorTree (BlockFollowedByTrap `inside` emptyContext)) <>
       WasmUnreachable
   else
       doNode dominatorTree emptyContext
 where
   dominatorTree :: Tree.Tree CmmBlock
   dominatorTree = fmap blockLabeled $ sortTree $ gwdDominatorTree gwd
   doNode     :: Tree.Tree CmmBlock     -> Context -> WasmControl s e
   nestWithin :: CmmBlock -> [Tree.Tree CmmBlock] -> Maybe Label -> Context -> WasmControl s e
   doBranch   :: Label -> Label         -> Context -> WasmControl s e

   treeEntryLabel = entryLabel . Tree.rootLabel

   doNode (Tree.Node x immediateDominatees) context =
       trace ("doNode " ++ showO (entryLabel x) ++ " " ++
                        showO (map treeEntryLabel $ immediateDominatees)) $
       let codeForX = nestWithin x dominatees Nothing
       in  if isHeader x then
             WasmLoop (codeForX context')
           else
             codeForX context
     where dominatees = case lastNode x of
                          CmmSwitch {} -> immediateDominatees
                          _ -> filter isMergeTree $ immediateDominatees
           context' = LoopHeadedBy (entryLabel x) `inside`
                        (context `withFallthrough` (entryLabel x))
     -- N.B. Dominatees must be ordered with largest RP number first.
     -- (In Peterson, this is case 1 step 2, which I do before step 1)

   nestWithin x (y_n:ys) (Just zlabel) context =
       WasmBlock $ nestWithin x (y_n:ys) Nothing context'
     where context' = BlockFollowedBy zlabel `inside` context
   nestWithin x (y_n:ys) Nothing context =
       nestWithin x ys (Just ylabel) (context `withFallthrough` ylabel) <> doNode y_n context
     where ylabel = treeEntryLabel y_n
   nestWithin x [] (Just zlabel) context
     | not (generatesIf x) =
         WasmBlock (nestWithin x [] Nothing context')
     where context' = BlockFollowedBy zlabel `inside` context
   nestWithin x [] maybeMarks context =
       trace ("translating " ++ showO (entryLabel x)) $
       translationOfX context

     -- (In Peterson, translationOfX combines case 1 step 6, case 2 step 1, case 2 step 3)
     where translationOfX :: Context -> WasmControl s e
           translationOfX context =
             WasmSlc (txBlock xlabel (nodeBody x)) <>
             case flowLeaving platform x of
               Unconditional l -> doBranch xlabel l context -- Peterson: case 1 step 6
               Conditional e t f -> -- Peterson: case 1 step 5
                 WasmIf (txExpr xlabel e)
                        (doBranch xlabel t (IfThenElse maybeMarks `inside` context))
                        (doBranch xlabel f (IfThenElse maybeMarks `inside` context))
               TerminalFlow -> WasmReturn
                  -- Peterson: case 1 step 6, case 2 steps 2 and 3
               Switch e range targets default' ->
                   WasmBrTable (trace "expr in switch" $ txExpr xlabel e)
                               range
                               (trace "map in switch" $ map switchIndex targets)
                               (switchIndex default')
            where switchIndex :: Maybe Label -> Int
                  switchIndex Nothing = nothingIndex
                  switchIndex (Just lbl) = index lbl (enclosing context)
                  nothingIndex = if paranoidFlow then trapIndex (enclosing context) else 0

           xlabel = entryLabel x
           -- headerStatus = if isHeader xlabel then " (HEADER)" else " (not header)"

   generatesIf x = case flowLeaving platform x of Conditional {} -> True
                                                  _ -> False

   -- In Peterson, `doBranch` implements case 2 (and part of case 1)
   doBranch from to context
      | to `elem` fallthrough context = mempty -- WasmComment "eliminated branch"
      | isBackward from to = WasmBr i -- continue
           -- Peterson: case 1 step 4
      | isMergeLabel to = WasmBr i -- exit
      | otherwise = doNode (subtreeAt to) context
     where i = index to (enclosing context)

   ---- everything else here is utility functions

   sortTree :: Tree.Tree Label -> Tree.Tree Label
     -- Sort highest rpnum first
   sortTree (Tree.Node label children) =
      Tree.Node label $ sortBy (flip compare `on` (rpnum . Tree.rootLabel)) $
                        map sortTree children


   subtreeAt :: Label -> Tree.Tree CmmBlock
   blockLabeled :: Label -> CmmBlock
   rpnum :: Label -> RPNum -- ^ reverse postorder number of the labeled block
   forwardPreds :: Label -> [Label] -- ^ reachable predecessors of reachable blocks,
                                    -- via forward edges only
   isMergeTree :: Tree.Tree CmmBlock -> Bool
   isMergeLabel :: Label -> Bool
   isMergeNode :: CmmBlock -> Bool
   isHeader :: CmmBlock -> Bool -- ^ identify loop headers
     -- ^ all nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the context first.
   dominates :: Label -> Label -> Bool
     -- ^ Domination relation (not just immediate domination)

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
   isMergeNode = isMergeLabel . entryLabel
   isMergeTree = isMergeNode . Tree.rootLabel

   subtreeAt label =
       mapFindWithDefault (panic "missing subtree") label subtrees
   subtrees :: LabelMap (Tree.Tree CmmBlock)
   subtrees = addSubtree mapEmpty dominatorTree
     where addSubtree map (t@(Tree.Node root children)) =
               foldl addSubtree (mapInsert (entryLabel root) t map) children

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

   isHeader = isHeaderLabel . entryLabel
   isHeaderLabel = \l -> setMember l headers  -- loop headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   index _ [] = panic "destination label not in context"
   index label (frame : context)
       | label `matchesFrame` frame = 0
       | otherwise = 1 + index label context
     where

   trapIndex :: [ContainingSyntax] -> Int
   trapIndex [] = panic "context does not include a trap"
   trapIndex (BlockFollowedByTrap : _) = 0
   trapIndex (_ : context) = 1 + trapIndex context


   gwd = graphWithDominators g
   rpnum = gwdRPNumber gwd
   dominates lbl blockname =
       lbl == blockname || dominatorsMember lbl (gwdDominatorsOf gwd blockname)

   isBackward from to = rpnum to <= rpnum from -- self-edge counts as a backward edge


flowLeaving :: Platform -> CmmBlock -> ControlFlow CmmExpr
flowLeaving platform b =
    case lastNode b of
      CmmBranch l -> Unconditional l
      CmmCondBranch c t f _ -> Conditional c t f
      CmmSwitch e targets ->
          trace "flow leaving CmmSwitch" $
          let (offset, target_labels) = switchTargetsToTable targets
              (lo, hi) = switchTargetsRange targets
              default_label = switchTargetsDefault targets
              scrutinee = smartPlus platform e offset
              range = inclusiveInterval (lo+toInteger offset) (hi+toInteger offset)
          in  Switch scrutinee range (atMost maximumSwitchSize target_labels) default_label

      CmmCall { cml_cont = Just l } -> Unconditional l
      CmmCall { cml_cont = Nothing } -> TerminalFlow
      CmmForeignCall { succ = l } -> Unconditional l

  where atMost :: Int -> [a] -> [a]
        atMost k xs = if xs `hasAtLeast` k then
                          panic "switch table is too big for WebAssembly"
                      else
                          xs

        hasAtLeast :: [a] -> Int -> Bool
        hasAtLeast _ 0 = True
        hasAtLeast [] _ = False
        hasAtLeast (_:xs) k = hasAtLeast xs (k - 1)

nodeBody :: Block n C C -> Block n O O
nodeBody (BlockCC _first middle _last) = middle


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
hasBlock p g = mapFoldl (\b block -> b || p block) False (graphMap g)

------------------------------------------------------------------
--- everything below here is for diagnostics in case of panic

instance Outputable ContainingSyntax where
    ppr (BlockFollowedBy l) = text "node" <+> ppr l
    ppr (LoopHeadedBy l) = text "loop" <+> ppr l
    ppr (IfThenElse l) = text "if-then-else" <+> ppr l
    ppr (BlockFollowedByTrap) = text "trap"
