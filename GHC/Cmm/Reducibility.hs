{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-matches #-}


module GHC.Cmm.Reducibility
  ( Reducibility(..)
  , reducibility

  , asReducible

  )
where

{-|
Module      : GHC.Cmm.Reducibility
Description : Tell if a `CmmGraph` is reducible, or make it so

Test a Cmm control-flow graph for reducibility.  And provide a
function that, when given an arbitrary control-flow graph, returns an
equivalent, reducible control-flow graph.  The equivalent graph is
obtained by "splitting" (copying) nodes of the original graph.
The resulting equivalent graph has the same dynamic behavior as the
original, but it is larger.

Documentation uses the language of control-flow analysis, in which a
basic block is called a "node."  These "nodes" are `CmmBlock`s or
equivalent; they have nothing to do with a `CmmNode`.

For more on reducibility and related analyses and algorithms, see
Note [Reducibility resources]
-}


import Prelude hiding (splitAt, succ)

import Data.List hiding (splitAt)
import Data.Maybe
import qualified Data.Sequence as Seq
import Control.Monad.State
import Control.Exception


import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph hiding (addBlock)
import qualified GHC.Cmm.Dataflow.Graph as G
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch
import GHC.Data.Graph.Collapse
import GHC.Data.Graph.Inductive.Graph
import GHC.Data.Graph.Inductive.PatriciaTree
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

import Debug.Trace


-- | Represents the result of a reducibility analysis.
data Reducibility = Reducible | Irreducible
  deriving (Eq, Show)

-- | Given a graph, say whether the graph is reducible.  The graph must
-- be bundled with a dominator analysis and a reverse postorder
-- numbering, as these results are needed to perform the test.

reducibility :: NonLocal node
             => GraphWithDominators node
             -> Reducibility
reducibility gwd =
    if all goodBlock blockmap then Reducible else Irreducible
  where goodBlock b = all (goodEdge (entryLabel b)) (successors b)
        goodEdge from to = rpnum to > rpnum from || to `dominates` from
        rpnum = gwdRPNumber gwd
        blockmap = graphMap $ gwd_graph gwd
        dominators = gwdDominatorsOf gwd
        dominates lbl blockname =
            lbl == blockname || dominatorsMember lbl (dominators blockname)

-- | Given a graph, return an equivalent reducible graph, by
-- "splitting" (copying) nodes if necessary.  The input
-- graph must be bundled with a dominator analysis and a reverse
-- postorder numbering.  The computation is monadic because when a
-- node is split, the new copy needs a fresh label.
--
-- Use this function whenever a downstream algorithm needs a reducible
-- control-flow graph.

asReducible :: GraphWithDominators CmmNode
            -> UniqSM (GraphWithDominators CmmNode)
asReducible gwd = case reducibility gwd of
                    Reducible -> return gwd
                    Irreducible -> assertReducible <$> nodeSplit gwd

assertReducible :: GraphWithDominators CmmNode -> GraphWithDominators CmmNode
assertReducible gwd = assert (reducibility gwd == Reducible) gwd

----------------------------------------------------------------

-- | Split one or more nodes of the given graph, which must be
-- irreducible.

tracePpr :: Outputable a => a -> b -> b
tracePpr = trace . showSDocUnsafe . ppr

nodeSplit :: GraphWithDominators CmmNode
          -> UniqSM (GraphWithDominators CmmNode)
nodeSplit gwd =
    graphWithDominators <$> inflate (g_entry g) <$> runNullCollapse collapsed
  where g = gwd_graph gwd
        collapsed :: NullCollapseViz (Gr CmmSuper ())
        collapsed = collapseInductiveGraph (cgraphOfCmm g)
{-
        initial :: GState
        initial = (predecessorMap blockmap, blockmap)
        blockmap = graphMap g
        -- split lower RP numbers first, so a node is split before its successors
        splits = take 1 $ sortOn (gwdRPNumber gwd) $ setElems $ labelsToSplit g
        -- see Note [Split count]

        comment doc = "/*" <+> doc <+> "*/"
        splitDecisions = comment "Splitting" $$
                         dotCFG maybeSplit "Graph to split" g
        maybeSplit block = if elem (entryLabel block) splits then
                               "SPLIT:" <+> ppr (entryLabel block)
                           else
                               ppr (entryLabel block)
-}

inflate :: Label -> CGraph -> CmmGraph
inflate = undefined

type CGraph = Gr CmmSuper ()

-- | Convert a `CmmGraph` into an inductive graph.
-- (The function coalesces duplicate edges into a single edge.)
cgraphOfCmm :: CmmGraph -> CGraph
cgraphOfCmm g = foldl' addSuccEdges (mkGraph cnodes []) blocks
   where blocks = zip [0..] $ revPostorderFrom (graphMap g) (g_entry g)
         cnodes = [(k, super block) | (k, block) <- blocks]
          where super block = Single (entryLabel block) (Seq.singleton block)
         labelNumber = \lbl -> fromJust $ mapLookup lbl numbers
             where numbers :: LabelMap Int
                   numbers = mapFromList $ map swap blocks
                   swap (k, block) = (entryLabel block, k)
         addSuccEdges :: CGraph -> (Node, CmmBlock) -> CGraph
         addSuccEdges graph (k, block) =
             insEdges [(k, labelNumber lbl, ()) | lbl <- nub $ successors block] graph


{-

nodeSplit' :: GraphWithDominators CmmNode
          -> UniqSM (GraphWithDominators CmmNode)
nodeSplit' gwd =
    tracePpr splitDecisions $
    graphWithDominators <$> evalStateT (foldM splitAt g splits) initial
  where g = gwd_graph gwd
        initial :: GState
        initial = (predecessorMap blockmap, blockmap)
        blockmap = graphMap g
        -- split lower RP numbers first, so a node is split before its successors
        splits = take 1 $ sortOn (gwdRPNumber gwd) $ setElems $ labelsToSplit g
        -- see Note [Split count]

        comment doc = "/*" <+> doc <+> "*/"
        splitDecisions = comment "Splitting" $$
                         dotCFG maybeSplit "Graph to split" g
        maybeSplit block = if elem (entryLabel block) splits then
                               "SPLIT:" <+> ppr (entryLabel block)
                           else
                               ppr (entryLabel block)
-}
------------------ Graph representation suitable for splitting

-- The `CmmGraph` representation provides quick access only to
-- successors, not to predecessors.  But the node-splitting algorithm
-- needs both.


type Predmap  = LabelMap LabelSet -- each block to set of predecessor blocks
type Blockmap = LabelMap CmmBlock -- each label to the block it labels
type GState = (Predmap, Blockmap) -- consistent picture of a graph


-- | Create an initial predecessor map.
predecessorMap :: Blockmap -> Predmap
predecessorMap bm = mapFoldl addBlock mapEmpty bm
  where addBlock map b = foldr (pmAddEdge from) map (successors b)
          where from = entryLabel b
        pmAddEdge from to = mapInsertWith setUnion to (setSingleton from)

------------- consistency checking

validated :: GState -> GState
validated s = assert (consistent s) s

consistent :: GState -> Bool
consistent (predmap, blockmap) =
    all (match predmap) (mapToList pbm) &&
    all (match pbm) (mapToList predmap)
  where pbm = predecessorMap blockmap
        match othermap (lbl, preds) = mapFindWithDefault setEmpty lbl othermap == preds

data Inconsistency = DifferentSets { _key :: Label, _preds :: LabelSet, _blocks :: LabelSet }
                   | NotInPredmap Label
                   | NotInBlockmap Label

showDoc :: Outputable a => a -> String
showDoc = showSDocUnsafe . ppr

instance Show Inconsistency where
  show (NotInBlockmap lbl) = showDoc lbl ++ " not in blockmap"
  show (NotInPredmap lbl) = showDoc lbl ++ " not in predmap"
  show (DifferentSets lbl preds blocks) =
      showDoc lbl ++ " has predmap " ++ showDoc preds ++ " but blockmap " ++ showDoc blocks


_faults :: GState -> [Inconsistency]
_faults (predmap, blockmap) = mapFoldlWithKey doPred missingPreds predmap
  where doPred faults lbl predset =
            case mapLookup lbl bpm of
              Nothing -> NotInBlockmap lbl : faults
              Just bps -> if bps /= predset then
                              DifferentSets lbl predset bps : faults
                          else
                              faults
        bpm = predecessorMap blockmap
        missingPreds = map NotInPredmap $ filter (not . flip mapMember predmap) $ mapKeys bpm


-- Operations on `Predmap`

pmAddEdge :: Label -> Label -> Predmap -> Predmap
pmAddEdge from to = mapInsertWith setUnion to (setSingleton from)

pmRemoveEdge :: Label -> Label -> Predmap -> Predmap
pmRemoveEdge from to = mapAdjust (remove from) to
  where remove from set = assert (setMember from set) $ setDelete from set


-- Operations on `GState`

addSuccessorEdges :: CmmBlock -> GState -> GState
addSuccessorEdges block (predmap, blockmap) =
    ( foldr (pmAddEdge (entryLabel block)) predmap (successors block)
    , blockmap
    )

rmSuccessorEdges :: CmmBlock -> GState -> GState
rmSuccessorEdges block (predmap, blockmap) =
    ( foldr (pmRemoveEdge (entryLabel block)) predmap (successors block)
    , blockmap
    )

addBlock :: CmmBlock -> GState -> GState
addBlock block =
    validated .
    addSuccessorEdges block .
    fmap (G.addBlock block)

changeBlock :: CmmBlock -> CmmBlock -> GState -> GState
changeBlock old new =
    fmap (mapAlter update (entryLabel old)) .
    addSuccessorEdges new .
    rmSuccessorEdges old
  where update Nothing = panic "no block to update"
        update (Just _) = Just new


-- Monadic operations to query the current `GState`

getBlockmap :: Monad m => StateT GState m Blockmap
getBlockmap = snd <$> get

predecessors :: Monad m => Label -> StateT GState m LabelSet
predecessors lbl = do predmap <- fst <$> get
                      return $ mapFindWithDefault die lbl predmap
  where die = panic "GHC.Cmm.Reducibility.predecessors"


------ the splitting algorithm


type UpdateMonad a = StateT GState UniqSM a

splitAt :: CmmGraph
        -> Label
        -> UpdateMonad CmmGraph
splitAt g lbl = do
    new_entry <- if g_entry g /= lbl then return $ g_entry g
                 else replicate (g_entry g)
    preds <- predecessors lbl
    mapM_ replaceIn $ setElems preds
    blockmap <- getBlockmap
    return $ CmmGraph new_entry (GMany NothingO blockmap NothingO)
   where replicate :: Label -> UpdateMonad Label
         replicate lbl = do b <- blockLabeled lbl <$> getBlockmap
                            lbl' <- lift newBlockId
                            let b' = relabel lbl' b
                            modify $ addBlock b'
                            return lbl'
         replaceIn :: Label -> UpdateMonad ()
         replaceIn plabel = do
             b <- blockLabeled plabel <$> getBlockmap
             new <- replicate lbl
             modify $ changeBlock b $ replaceSuccessor lbl new b

-- In `block`, replace outgoing edges to `old` with corresponding edges to `new`.
replaceSuccessor :: Label -> Label -> CmmBlock -> CmmBlock
replaceSuccessor old new block = blockJoinTail head tail'
  where (head, tail) = blockSplitTail block
        tail' = update tail
        redirect lbl = if lbl == old then new else lbl
        single lbl   = if lbl == old then new
                       else panic "GHC.Cmm.Reducibility.replaceSuccessor"
        update (c@CmmCall {cml_cont = succ}) = c { cml_cont = fmap single succ }
        update (c@CmmForeignCall {succ = s}) = c { succ = single s }
        update (CmmSwitch e targets) = CmmSwitch e (mapSwitchTargets redirect targets)
        update (CmmBranch lbl) = CmmBranch (single lbl)
        update (CmmCondBranch e t f likely) =
            CmmCondBranch e (redirect t) (redirect f) likely


blockLabeled :: Label -> Blockmap -> CmmBlock
blockLabeled = mapFindWithDefault (panic "GHC.Cmm.Reducibility")

relabel :: Label -> CmmBlock -> CmmBlock
relabel lbl b = blockJoinHead (CmmEntry lbl scope) tail
  where (CmmEntry _ scope, tail) = blockSplitHead b

{-

Note [Reducibility resources]
-----------------------------

*Flow Analysis of Computer Programs.* Matthew S. Hecht North Holland, 1977.
Available to borrow from archive.org.

Matthew S. Hecht and Jeffrey D. Ullman (1972).
Flow Graph Reducibility. SIAM J. Comput., 1(2), 188–202.
https://doi.org/10.1137/0201014

Johan Janssen and Henk Corporaal. 1997. Making graphs reducible with
controlled node splitting. ACM TOPLAS 19, 6 (Nov. 1997),
1031–1052. DOI:https://doi.org/10.1145/267959.269971

https://rgrig.blogspot.com/2009/10/dtfloatleftclearleft-summary-of-some.html
 (Nice summary of useful facts)

-}

{-

Note [Split count]
------------------

The collapsing algorithm of Hecht and Ullman produces a set of nodes
to split.  If a loop has two entry points, the set contains one of
them: the "designated" entry point.  The set also contains every node
that got absorbed into the designated entry point.  Nodes on the path
from the designated entry point to another entry point *must* be
split.  But the collapsing algorithm may well absorb other nodes that
need *not* be split---for example, nodes on the path from the
designated entry point to the exit node.  For details see
Note [Collapse estimate].

When the collapsing algorithm produces a set containing more than one
node, I split only the node with the smallest reverse postorder
number.  This choice minimizes the size of the generated code, but at
the cost of more collapsing passes.  The net effect on compile time is
unknown: time saved not generating code for additional nodes might or
might not pay for the additional passes.  Since the algorithm is
believed to be invoked very rarely, I have not investigated further.

It would be possible to choose differently simply by replacing `take 1`
with some other function of the list of nodes.  "Take all" is one
possibility, as is an iterative algorithm in which the number of nodes
taken doubles at every iteration, guaranteeing some savings in code
size while limiting the number of passes to logarithmic.

-}




type Seq = Seq.Seq

data CmmSuper
    = Single { label :: Label
             , blocks :: Seq (Block CmmNode C C)
             }
    | Consumed { label :: Label -- of the merged supernode
               , blocks :: Seq (Block CmmNode C C)
               }

instance Semigroup CmmSuper where
  s <> s' = Consumed (label s) (blocks s <> blocks s')

instance PureSupernode CmmSuper where
  superLabel = label
  mapLabels = changeLabels

instance Supernode CmmSuper NullCollapseViz where
  freshen s = liftUniqSM $ relabel' s

changeLabels :: (Label -> Label) -> (CmmSuper -> CmmSuper)
changeLabels _f _ = panic "change labels"

definedLabels :: CmmSuper -> Seq Label
definedLabels = fmap entryLabel . blocks


relabel' :: CmmSuper -> UniqSM (CmmSuper, Label -> Label)
relabel' = relabel''
{-
relabel' node = do
     mapping <- sequence pairs
     return $ (changeLabels (labelChanger mapping) node, labelChanger mapping)
  where oldLabels = definedLabels node
        pairs = [do { new <- newBlockId; return (old, new) } | old <- toList oldLabels]
        labelChanger :: [(Label, Label)] -> (Label -> Label)
        labelChanger mapping =
            let map = mapFromList mapping :: LabelMap Label
            in  \lbl -> mapFindWithDefault lbl lbl map
-}

relabel'' :: CmmSuper -> UniqSM (CmmSuper, Label -> Label)
relabel'' node = do
     finite_map <- foldM addPair mapEmpty $ definedLabels node
     return $ (changeLabels (labelChanger finite_map) node, labelChanger finite_map)
  where addPair :: LabelMap Label -> Label -> UniqSM (LabelMap Label)
        addPair map old = do new <- newBlockId
                             return $ mapInsert old new map
        labelChanger :: LabelMap Label -> (Label -> Label)
        labelChanger mapping = \lbl -> mapFindWithDefault lbl lbl mapping

{-
reverse :: CmmSuper -> [Block CmmNode C C]
reverse (Single lbl block succs) = [splice block succs `withLabel` lbl]
reverse (Consumed lbl consumer consumed edge_indices successors) =
  panic "not yet"
reverse (SelfEdge {}) = panic "not yet"

withLabel :: Block CmmNode C C -> Label -> Block CmmNode C C
withLabel block lbl = blockJoinHead entry' tail
    where (CmmEntry _ scope, tail) = blockSplitHead block
          entry' = CmmEntry lbl scope

splice :: Block CmmNode e C -> [Label] -> Block CmmNode e C
splice block labels = spliceIndexed block $ zip [0..] labels

spliceIndexed :: Block CmmNode e C -> [(Int, Label)] -> Block CmmNode e C
spliceIndexed block ilabels = panic "splice not implemented"
-}
