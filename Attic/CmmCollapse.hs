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
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dominators

import qualified GHC.Data.Graph.Inductive.PatriciaTree as PT
import GHC.Data.Graph.Inductive
import GHC.Wasm.ControlFlow.Collapse (Info(..), VizCollapseMonad(..), collapse)

import GHC.Utils.Panic


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




type CGraph = PT.Gr Info ()

labelsToSplit :: CmmGraph -> LabelSet
labelsToSplit = unSplit . collapse . cgraphOfCmm
  where unSplit m = getSplits $ evalState m emptyVs
        getSplits g = case labNodes g of
                        [(_, info)] -> splitLabels info
                        _ -> panic "GHC.Cmm.Collapse.labelsToSplit"

collapseCmm :: CmmGraph -> [CollapseEvent]
collapseCmm = unViz . collapse . cgraphOfCmm

unViz :: State VS a -> [CollapseEvent]
unViz m = reverse $ unVS $ execState m emptyVs


data CollapseEvent = ConsumeBy Node Node CGraph
           | SplitAt CGraph Node
           | Finish CGraph

newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

newtype VS = VS { unVS :: [CollapseEvent] }

add event (VS events) = VS (event : events)
add :: CollapseEvent -> VS -> VS


emptyVs :: VS
emptyVs = VS  []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizCollapseMonad (State VS) PT.Gr where
  consumeByInGraph to from g = modify (add $ ConsumeBy to from g)
  splitGraphAt g (k, _) = modify (add $ SplitAt g k)
  finalGraph g = modify (add $ Finish g)
