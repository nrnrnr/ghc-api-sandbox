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
import Control.Exception

import GHC.Utils.Panic

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
import GHC.Utils.Outputable
--import GHC.Utils.Misc

import GHC.Types.Unique.Supply

import Debug.Trace

data Reducibility = Reducible | Irreducible
  deriving (Eq, Show)

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
        splits = trace ("initial predmap " ++ showPredmap (fst initial)) $
                 take 1 $
                 sortOn (gwdRPNumber gwd) $ setElems $ showLabels $ labelsToSplit g
        showLabels lbls = trace ("splitting nodes labeled " ++ showDoc lbls) lbls
{-
  1. Form shadow graph
  2. Accumulate nodes that must be split
  3. Split the original graph
  4. call asReducible again
-}

showPredmap :: Predmap -> String
showPredmap p =
    intercalate "," $
    [ showDoc lbl ++ ": " ++ showDoc preds | (lbl, preds) <- mapToList p]

type Predmap = LabelMap LabelSet -- each block to set of predecessor blocks
type Blockmap = LabelMap CmmBlock

pmAddEdge :: Label -> Label -> Predmap -> Predmap
pmAddEdge from to = trace ("add edge " ++ showDoc from ++ " -> " ++ showDoc to) $
                    mapInsertWith setUnion to (setSingleton from)

pmRemoveEdge :: Label -> Label -> Predmap -> Predmap
pmRemoveEdge from to = trace ("remove edge " ++ showDoc from ++ " -> " ++ showDoc to) $
                       mapAdjust (remove from) to
  where remove from set = assert (setMember from set) $ setDelete from set


predecessorMap :: Blockmap -> Predmap
predecessorMap bm = mapFoldl addBlock mapEmpty bm
  where addBlock map b = foldr (pmAddEdge from) map (successors b)
          where from = entryLabel b
        pmAddEdge from to = mapInsertWith setUnion to (setSingleton from)

type GState = (Predmap, Blockmap)

validated :: GState -> GState
validated s =
    if not (consistent s) then
        trace (show $ faults s) $
        trace ("? predmap == " ++ showPredmap (fst s)) $
        trace ("? blockMap == " ++ showPredmap (predecessorMap (snd s))) $
        assert (consistent s) s
    else
        trace ("GOOD predmap  == " ++ showPredmap (fst s)) $
        trace ("     blockMap == " ++ showPredmap (predecessorMap (snd s))) $
        s

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


faults :: GState -> [Inconsistency]
faults (predmap, blockmap) = mapFoldlWithKey doPred missingPreds predmap
  where doPred faults lbl predset =
            case mapLookup lbl bpm of
              Nothing -> NotInBlockmap lbl : faults
              Just bps -> if bps /= predset then
                              DifferentSets lbl predset bps : faults
                          else
                              faults
        bpm = predecessorMap blockmap
        missingPreds = map NotInPredmap $ filter (not . flip mapMember predmap) $ mapKeys bpm


type UpdateMonad a = StateT GState UniqSM a



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
    trace ("adding block " ++ showDoc (entryLabel block)) .
    validated .
--    liftFst (mapInsert (entryLabel block) setEmpty) .
    addSuccessorEdges block .
    fmap (G.addBlock block)


_delBlock :: CmmBlock -> GState -> GState
_delBlock block s@(predmap, blockmap) =
  assert (mapMember lbl predmap) $
  assert (mapMember lbl blockmap) $
  trace ("deleting block " ++ showDoc (entryLabel block)) $
  -- validated -- invariant may be violated temporarily here
  rmSuccessorEdges block $
  fmap (mapDelete lbl) $
  s
 where lbl = entryLabel block

changeBlock :: CmmBlock -> CmmBlock -> GState -> GState
changeBlock old new =
    trace ("changing block " ++ showDoc (entryLabel old) ++ ": " ++ showDoc (successors old) ++ " -> " ++ showDoc (successors new)) .
    fmap (mapAlter update (entryLabel old)) .
    addSuccessorEdges new .
    rmSuccessorEdges old
  where update Nothing = panic "no block to update"
        update (Just _) = Just new


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
                            modify $ (trace ("replicating " ++ showDoc lbl ++ " as " ++ showDoc lbl') . addBlock b')
                            return lbl'
         replaceIn :: Label -> UpdateMonad ()
         replaceIn plabel = do
             b <- blockLabeled plabel <$> getBlockmap
             new <- replicate lbl
             modify $ changeBlock b $
                    trace ("replace " ++ l lbl ++ " with " ++ l new ++ " in " ++ l plabel) $
                    replaceSuccessor lbl new b
         l = showDoc

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
