{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Cmm.Collapse
  ( collapseCmm
  , Event(..)
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
import qualified GHC.Data.Graph.Inductive as IG
import GHC.Wasm.ControlFlow.Collapse (Info(..), VizMonad(..), collapse)


cgraphOfCmm :: CmmGraph -> CGraph
cgraphOfCmm g = foldl' addSuccEdges (IG.mkGraph cnodes []) blocks
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
         addSuccEdges :: CGraph -> IG.LNode CmmBlock -> CGraph
         addSuccEdges graph (k, block) =
             IG.insEdges [(k, labelNumber lbl, ()) | lbl <- successors block] graph
         rpnums = gwd_rpnumbering $ graphWithDominators g
         rpnum lbl = fromJust $ mapLookup lbl rpnums




type CGraph = PT.Gr Info ()
type Node = IG.Node

collapseCmm :: CmmGraph -> [Event]
collapseCmm = unViz . collapse . cgraphOfCmm

unViz :: State VS a -> [Event]
unViz m = reverse $ unVS $ execState m emptyVs


data Event = ConsumeBy Node Node CGraph
           | SplitAt CGraph Node
           | Finish CGraph

newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

newtype VS = VS { unVS :: [Event] }

add event (VS events) = VS (event : events)
add :: Event -> VS -> VS


emptyVs :: VS
emptyVs = VS  []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizMonad (State VS) PT.Gr where
  consumeByInGraph to from g = modify (add $ ConsumeBy to from g)
  splitGraphAt g (k, _) = modify (add $ SplitAt g k)
  finalGraph g = modify (add $ Finish g)
