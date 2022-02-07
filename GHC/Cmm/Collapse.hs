{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Cmm.Collapse
  ( collapseCmm
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

collapseCmm :: CmmGraph -> [(CGraph, IG.LNode Info, CGraph)]
collapseCmm = unViz . collapse . cgraphOfCmm

unViz :: State VS a -> [(CGraph, IG.LNode Info, CGraph)]
unViz m = case execState m emptyVs of
            VS { initg = Nothing, split_node = Nothing, finals = fs } -> fs
            _ -> error "viz monad in bad state"


newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

data VS = VS { initg :: Maybe CGraph, split_node :: Maybe (IG.LNode Info)
             , finals :: [(CGraph, IG.LNode Info, CGraph)]
             }

emptyVs :: VS
emptyVs = VS Nothing Nothing []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizMonad (State VS) PT.Gr where
  initialGraph g = modify (\s -> if isNothing (initg s) then s { initg = Just g }
                                 else error "double init")
  splitGraphAt _g ln =
      modify (\s -> if isJust (initg s) && isNothing (split_node s) then
                        s { split_node = Just ln }
                    else error "double split")
  finalGraph g' =
      modify (\case VS { initg = Just g, split_node = Just n, finals = fs } ->
                        VS { initg = Nothing, split_node = Nothing
                           , finals = (g, n, g') : fs
                           }
                    _ -> error "final too early")
