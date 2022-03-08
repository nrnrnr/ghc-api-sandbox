{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Cmm.Collapse
  ( collapseCmm
  , labelsToSplit
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

import qualified GHC.Data.Graph.Inductive.PatriciaTree as PT
import GHC.Data.Graph.Inductive
import GHC.Wasm.ControlFlow.Collapse (Info(..), VizCollapseMonad(..), collapse)

import GHC.Utils.Panic

type CGraph = PT.Gr Info ()

-- | Convert a `CmmGraph` into something usable with FGL.

cgraphOfCmm :: CmmGraph -> CGraph -- must avoid duplicates!
cgraphOfCmm g = foldl' addSuccEdges (mkGraph cnodes []) blocks
   where blocks = zip [0..] $ revPostorderFrom (graphMap g) (g_entry g)
         cnodes = [(k, info block) | (k, block) <- blocks]
          where info block = Info { unsplitLabels = setSingleton (entryLabel block)
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
         rpnums = gwd_rpnumbering $ graphWithDominators g
         rpnum lbl = fromJust $ mapLookup lbl rpnums

-- | Find all the nodes that were split during collapse.
-- This set can be an overestimate; see Note [Collapse Estimate]
labelsToSplit :: CmmGraph -> LabelSet
labelsToSplit = getSplits . unNV . collapse . cgraphOfCmm
  where getSplits g = case labNodes g of
                        [(_, info)] -> splitLabels info
                        _ -> panic "GHC.Cmm.Collapse.labelsToSplit"

newtype NullViz a = NV { unNV :: a }

instance Functor NullViz where
  fmap f (NV a) = NV (f a)

instance Applicative NullViz where
  pure = NV
  NV f <*> NV a = NV (f a)

instance Monad NullViz where
  return = pure
  NV a >>= k = k a

instance Graph gr => VizCollapseMonad NullViz gr where
  consumeByInGraph _ _ _ = return ()
  splitGraphAt _ _ = return ()
  finalGraph _ = return ()

----------------

-- | Run the same collapsing algorithm, but this time return a
-- visualization.
collapseCmm :: CmmGraph -> [CollapseEvent]
collapseCmm = unViz . collapse . cgraphOfCmm

data CollapseEvent = ConsumeBy Node Node CGraph
           | SplitAt CGraph Node
           | Finish CGraph

newtype VS = VS { unVS :: [CollapseEvent] }

unViz :: State VS a -> [CollapseEvent]
unViz m = reverse $ unVS $ execState m emptyVs

newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

emptyVs :: VS
emptyVs = VS  []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizCollapseMonad (State VS) PT.Gr where
  consumeByInGraph to from g = modify (add $ ConsumeBy to from g)
  splitGraphAt g (k, _) = modify (add $ SplitAt g k)
  finalGraph g = modify (add $ Finish g)

add event (VS events) = VS (event : events)
add :: CollapseEvent -> VS -> VS


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
