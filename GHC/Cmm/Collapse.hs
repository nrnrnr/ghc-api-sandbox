{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Cmm.Collapse
  ( -- * Node splitting
    labelsToSplit
    -- * Visualization
  , collapseCmm
  , CollapseEvent(..)
  )
where

import Control.Monad.State
import Data.List
import Data.Maybe

import GHC.Cmm
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph hiding (Graph)
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dominators

import GHC.Data.Graph.Collapse
import GHC.Data.Graph.Inductive.Graph
import GHC.Data.Graph.Inductive.PatriciaTree

import GHC.Utils.Panic

type CGraph = Gr CollapseInfo ()

-- | Call this function to find a set of candidates for node
-- splitting.  It runs the Hecht/Ullman collapsing algorithm and
-- returns all the nodes that were split during collapse.
-- You may want to split fewer tnodes than are in this set;
-- see Note [Collapse Estimate].
labelsToSplit :: CmmGraph -> LabelSet
labelsToSplit = getSplits . unNCV . collapseInductiveGraph . cgraphOfCmm
  -- N.B. `unNCV` forces this function to use the identity monad.
  where getSplits g = case labNodes g of
                        [(_, info)] -> splitLabels info
                        _ -> panic "GHC.Cmm.Collapse.labelsToSplit"

-- | Convert a `CmmGraph` into an inductive graph.
-- (The function coalesces duplicate edges into a single edge.)
cgraphOfCmm :: CmmGraph -> CGraph
cgraphOfCmm g = foldl' addSuccEdges (mkGraph cnodes []) blocks
   where blocks = zip [0..] $ revPostorderFrom (graphMap g) (g_entry g)
         cnodes = [(k, info block) | (k, block) <- blocks]
          where info block =
                  CollapseInfo { unsplitLabels = setSingleton (entryLabel block)
                               , splitLabels = setEmpty
                               , rpnumber = rpnum (entryLabel block)
                               }
         labelNumber = \lbl -> fromJust $ mapLookup lbl numbers
             where numbers :: LabelMap Int
                   numbers = mapFromList $ map swap blocks
                   swap (k, block) = (entryLabel block, k)
         addSuccEdges :: CGraph -> LNode CmmBlock -> CGraph
         addSuccEdges graph (k, block) =
             insEdges [(k, labelNumber lbl, ()) | lbl <- nub $ successors block] graph
         rpnum = gwdRPNumber $ graphWithDominators g


----------------

-- | Call this function to get a visualization of the Hecht/Ullman
-- collapsing algorithm, with splitting.  The visualization gives the
-- sequence of notable events, in order of occurrence.
collapseCmm :: CmmGraph -> [CollapseEvent]
collapseCmm = unViz . collapseInductiveGraph . cgraphOfCmm

-- | Steps that occur during the Hecht/Ullman algorithm.
data CollapseEvent
  = ConsumeBy Node Node CGraph -- ^ In the given graph, a node is
                               -- consumed (absorbed) by its unique
                               -- predecessor (and any resulting
                               -- self-edges are removed).

  | SplitAt CGraph Node        -- ^ The graph is split at the given node.

  | Finish CGraph              -- ^ The algorithm terminates,
                               -- producing the given graph.

-- | State for a state monad: all the events that have occurred, most
-- recent event first.
newtype VS = VS { unVS :: [CollapseEvent] }

unViz :: State VS a -> [CollapseEvent]
unViz m = reverse $ unVS $ execState m emptyVs

-- | Visualization monad.  Accumulates events.
newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

emptyVs :: VS
emptyVs = VS  []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizCollapseMonad (State VS) Gr where
  consumeByInGraph to from g = modify (add $ ConsumeBy to from g)
  splitGraphAt g (k, _) = modify (add $ SplitAt g k)
  finalGraph g = modify (add $ Finish g)

add :: CollapseEvent -> VS -> VS
add event (VS events) = VS (event : events)


-- Note [Collapse Estimate]
--
-- The collapse algorithm splits a node when necessary, but at the
-- time the nodes is split it may have already absorbed some of its
-- successors.  For an example, consider this variation on the classic
-- irreducible graph:
--
--
--     ENTRY
--      /  \
--     A -> B
--       <-  \
--           C
--           |
--          EXIT
--
-- If B is split here, it will be split after absorbing C, so C will
-- be in the set of split labels.  But only B needs to be duplicated;
-- once we have B and B', both of them can share the pointer to C.
-- (But for example if there were other nodes on the path from B to A,
-- those nodes *would* have to be duplicated.)
--
-- If what's desired is to minimize the size of generated code, the
-- safe bet is to split only the smallest (by RP number) of the nodes
-- in the split-label set.  If what's desired is to minimize the
-- number of splitting passes, the safe bet is to split *all* the
-- nodes in the split-label set.
--
-- See also Note [Split count]
