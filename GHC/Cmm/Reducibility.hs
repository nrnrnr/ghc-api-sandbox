{-# LANGUAGE GADTs #-}

module GHC.Cmm.Reducibility
  ( Reducibility(..)
  , reducibility

  , asReducible


  , Node(..) -- temporary

  , splitAt -- temporary

  )
where

-- see Note [Reducibility resources]

import Prelude hiding (splitAt, succ)

import Data.List hiding (splitAt)


import Control.Monad.State

import GHC.Utils.Panic

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch

import GHC.Types.Unique.Supply

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


asReducible :: NonLocal node
            => GraphWithDominators node
            -> UniqSM (GraphWithDominators node)
asReducible gwd = case reducibility gwd of
                    Reducible -> return gwd
                    Irreducible -> nodeSplit gwd

nodeSplit :: NonLocal node
          => GraphWithDominators node
          -> UniqSM (GraphWithDominators node)
nodeSplit = panic "GHC.Cmm.Reducibility: nodesplit unimp" $ swallow
{-
  1. Form shadow graph
  2. Accumulate nodes that must be split
  3. Split the original graph
  4. call asReducible again
-}

type BlockMap = LabelMap (Block CmmNode C C)

type UpdateMonad a = StateT BlockMap UniqSM a

splitAt :: LabelMap [Label]
        -> CmmGraph
        -> Label
        -> UpdateMonad CmmGraph
splitAt predmap g lbl = do
    new_entry <- if g_entry g /= lbl then return $ g_entry g
                 else replicate (g_entry g)
    mapM_ replaceIn (predecessors lbl)
    blockmap <- get
    return $ CmmGraph new_entry (GMany NothingO blockmap NothingO)
   where replicate :: Label -> UpdateMonad Label
         replicate lbl = do b <- blockLabeled lbl <$> get
                            lbl' <- lift newBlockId
                            let b' = relabel lbl' b
                            modify $ addBlock b'
                            return lbl'
         replaceIn plabel = do
             b <- blockLabeled plabel <$> get
             new <- replicate lbl
             modify $ addBlock $ replaceSuccessor lbl new b

         predecessors lbl = mapFindWithDefault (panic "GHC.Cmm.Reducibility") lbl predmap

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



blockLabeled :: Label -> BlockMap -> CmmBlock
blockLabeled = mapFindWithDefault (panic "GHC.Cmm.Reducibility")

relabel :: Label -> CmmBlock -> CmmBlock
relabel lbl b = blockJoinHead (CmmEntry lbl scope) tail
  where (CmmEntry _ scope, tail) = blockSplitHead b

data XLabel = Original Label
            | Replica Int XLabel
  deriving (Eq, Ord)

data Node = Node { label :: XLabel
                 , also :: [XLabel]
                 , xsuccessors :: [XLabel]
                 , predecessors :: [Node] -- bogus
                 }

swallow :: Node -> Node -> Node
swallow u v =
    case predecessors v of
      [u'] | label u' == label u -> u { also = label u' : also u' ++ also u
                                      , xsuccessors = (xsuccessors u \\ [label v]) ++
                                                      (xsuccessors v \\ [label u])
                                      }
                                    -- URK! successors of v get new predecessor
      _ -> panic "swallow, bad predecessors"


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
