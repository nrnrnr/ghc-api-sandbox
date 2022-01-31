{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Cmm.Collapse
--  ( collapseCmm
--  )
where

import Control.Monad.State
import Data.Maybe

import GHC.Cmm

import qualified GHC.Data.Graph.Inductive.PatriciaTree as PT
import qualified GHC.Data.Graph.Inductive as IG
import GHC.Wasm.ControlFlow.Collapse (Info(..), VizMonad(..))


cgraphOfCmm :: CmmGraph -> CGraph
cgraphOfCmm g = foldl' addSuccEdges (mkGraph cnodes []) cnodes
   where cnodes = zipWith labelNode [0..] (revPostorderFrom blockmap (g_entry g))
         labelNode k block = (k, info)
          where info = Info { unsplitLabels = setSingleton (entryLabel block)
                            , splitLabels = setEmpty
                            , rpnumber = k
                            }
         addSuccEdges graph block = insEdges undefined

graphMap :: GenCmmGraph n -> LabelMap (Block n C C)
graphMap (CmmGraph { g_graph = GMany NothingO blockmap NothingO }) = blockmap





type CGraph = PT.Gr Info ()

collapseCmm :: CmmGraph -> [(CGraph, IG.LNode Info, CGraph)]
collapseCmm = undefined

newtype VM a = VM { unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

data VS = VS { initg :: Maybe CGraph, split_node :: Maybe (IG.LNode Info)
             , finals :: [(CGraph, IG.LNode Info, CGraph)]
             }

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
                  
