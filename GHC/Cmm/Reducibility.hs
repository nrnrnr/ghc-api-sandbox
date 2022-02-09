{-# LANGUAGE GADTs #-}

module GHC.Cmm.Reducibility
  ( Reducibility(..)
  , reducibility

  , asReducible
  )
where

-- see Note [Reducibility resources]

import Prelude hiding (splitAt, succ)

import Data.List hiding (splitAt)


import Control.Monad.State

import GHC.Utils.Panic

import GHC.Base hiding (foldr)
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Collapse
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph hiding (addBlock)
import qualified GHC.Cmm.Dataflow.Graph as G
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch

import GHC.Types.Unique.Supply

import Debug.Trace

data Reducibility = Reducible | Irreducible

reducibility :: NonLocal node
             => GraphWithDominators node
             -> Reducibility
reducibility gwd =
    if all goodBlock blockmap then Reducible else Irreducible
  where goodBlock b = unreachable b || all (goodEdge (entryLabel b)) (successors b)
        goodEdge from to = rpnum to > rpnum from || to `dominates` from
        unreachable b = rpnum (entryLabel b) == unreachableRPNum

        rpnum = gwdRPNumber gwd
        blockmap = graphMap $ gwd_graph gwd
        dominators = gwdDominatorsOf gwd
        dominates lbl blockname =
            lbl == blockname || dominatorsMember lbl (dominators blockname)


asReducible :: GraphWithDominators CmmNode
            -> UniqSM (GraphWithDominators CmmNode)
asReducible gwd = case reducibility gwd of
                    Reducible -> return gwd
                    Irreducible -> trace "splitting" $ nodeSplit gwd >>= asReducible

----------------------------------------------------------------



nodeSplit :: GraphWithDominators CmmNode
          -> UniqSM (GraphWithDominators CmmNode)
nodeSplit gwd =
    graphWithDominators <$> evalStateT (foldM splitAt g splits) initial
  where g = gwd_graph gwd
        initial :: GState
        initial = (predecessorMap blockmap, blockmap)
        blockmap = graphMap g
        -- split lower RP numbers first, so a node is split before its successors
        splits = sortOn (gwdRPNumber gwd) $ setElems $ labelsToSplit g

{-
  1. Form shadow graph
  2. Accumulate nodes that must be split
  3. Split the original graph
  4. call asReducible again
-}

type Predmap = LabelMap LabelSet -- each block to set of predecessor blocks
type Blockmap = LabelMap CmmBlock

pmAddEdge :: Label -> Label -> Predmap -> Predmap
pmAddEdge from to = mapInsertWith setUnion to (setSingleton from)

pmRemoveEdge :: Label -> Label -> Predmap -> Predmap
pmRemoveEdge from to = mapAdjust (remove from) to
  where remove from set = assert (setMember from set) $ setDelete from set


predecessorMap :: Blockmap -> Predmap
predecessorMap bm = mapFoldl addBlock mapEmpty bm
  where addBlock map b = foldr (pmAddEdge from) map (successors b)
          where from = entryLabel b

type GState = (Predmap, Blockmap)

validated :: GState -> GState
validated s = assert (consistent s) s

consistent :: GState -> Bool
consistent (predmap, blockmap) = predmap == predecessorMap blockmap

type UpdateMonad a = StateT GState UniqSM a

addBlock :: CmmBlock -> GState -> GState
addBlock block (predmap, blockmap) =
    validated
    ( foldr (pmAddEdge (entryLabel block)) predmap (successors block)
    , G.addBlock block blockmap
    )

delBlock :: CmmBlock -> GState -> GState
delBlock block (predmap, blockmap) =
  assert (mapMember lbl predmap) $
  assert (mapMember lbl blockmap) $
  validated
  ( foldr (pmRemoveEdge lbl) (mapDelete lbl predmap) (successors block)
  , mapDelete lbl blockmap
  )
 where lbl = entryLabel block

changeBlock :: CmmBlock -> GState -> GState
changeBlock block = addBlock block . delBlock block

getBlockmap :: Monad m => StateT GState m Blockmap
getBlockmap = snd <$> get

predecessors :: Monad m => Label -> StateT GState m LabelSet
predecessors lbl = do predmap <- fst <$> get
                      return $ mapFindWithDefault die lbl predmap
  where die = panic "GHC.Cmm.Reducibility.predecessors"


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
             modify $ changeBlock $ replaceSuccessor lbl new b


replaceSuccessor :: Label -> Label -> CmmBlock -> CmmBlock
replaceSuccessor old new block = blockJoinTail head tail'
  where (head, tail) = blockSplitTail block
        tail' = update tail
        redirect lbl = if lbl == old then new else lbl
        single lbl = if lbl == old then new else panic "GHC.Cmm.Reducibility.replaceSuccessor"
        update (CmmBranch lbl) = CmmBranch (single lbl)
        update (CmmCondBranch e t f likely) = CmmCondBranch e (redirect t) (redirect f) likely
        update (CmmSwitch e targets) = CmmSwitch e (mapSwitchTargets redirect targets)
        update (c@CmmCall {cml_cont = succ}) = c { cml_cont = fmap single succ }
        update (c@CmmForeignCall {succ = s}) = c { succ = single s }



blockLabeled :: Label -> Blockmap -> CmmBlock
blockLabeled = mapFindWithDefault (panic "GHC.Cmm.Reducibility")

relabel :: Label -> CmmBlock -> CmmBlock
relabel lbl b = blockJoinHead (CmmEntry lbl scope) tail
  where (CmmEntry _ scope, tail) = blockSplitHead b

data XLabel = Original Label
            | Replica Int XLabel
  deriving (Eq, Ord)
{-

data Node = Node { label :: XLabel
                 , also :: [XLabel]
                 , xsuccessors :: [XLabel]
                 , predecessors :: [Node] -- bogus
                 }
-}


{- Note [Reducibility resources]

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
