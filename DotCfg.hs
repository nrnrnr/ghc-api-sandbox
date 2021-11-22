{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DotCfg (
  dotCFG  
)
where

import Data.List
import Prelude hiding ((<>))

import GHC.Cmm
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable


                     

dotCFG :: forall node . (NonLocal node, node ~ CmmNode) => SDoc -> GenCmmGraph node -> SDoc
dotCFG title (g@CmmGraph { g_graph = GMany NothingO blockmap NothingO, g_entry = entry }) =
    -- blockmap is foldable and traversable
    text "digraph {" $$
    nest 2 (edges $$
            nodes $$
            text "entry [shape=\"plaintext\", label=" <>
                 doubleQuotes title <> text "];" $$
            text "entry -> " <> dotName entry <> text ";")
    $$
    text "}"
  where edges = vcat $ map dotEdge $ mapFoldMapWithKey outedges blockmap
        nodes = vcat $ map (dotNode dmap) $ mapKeys blockmap
        outedges :: Label -> Block node C C -> [(Label, Label)]
        outedges label block = map (label,) $ successors block
        dmap = dominators g
--        headers = loopHeaders

dotNode :: LabelMap DominatorSet -> Label -> SDoc
dotNode dmap label =
  dotName label <> space <>
  text "[label=" <> doubleQuotes dotlabel <> space <> text "]"
                <> text ";"
  where dotlabel = dotName label <> text ": " <> 
                   (hcat $ intersperse (comma<>space) $ map dotName $ setElems $ getFact nodeset label dmap)

dotEdge :: (Label, Label) -> SDoc
dotEdge (from, to) = dotName from <> text "->" <> dotName to <> text ";"

dotName :: Label -> SDoc
dotName label = text ("L" ++ shortdigits)
  where shortdigits = case show label of
                        'L' : digits -> show ((read digits :: Int) `mod` 512)
                        urk -> urk


--targets :: Block CmmNode e C -> [Label]
----targets _ = [mkHooplLabel 99]
--targets (CmmBranch target) = [target]
--targets (CmmCondBranch { cml_true = t, cml_false = f }) = [t, f]
--targets (CmmSwitch _ ts) = panic "switch targets not implemented"
--targets (CmmCall { cml_cont = k }) = toList k
--targets (CmmForeignCall { succ = l }) = [l]

type DominatorSet = LabelSet

nodeset :: DataflowLattice DominatorSet
nodeset = DataflowLattice setEmpty join
  where join (OldFact old) (NewFact new) =
            (if inter == old then NotChanged else Changed) inter
          where inter = setIntersection old new
                        -- probably could be done more efficiently

transfer :: TransferFun DominatorSet
transfer block facts =
    asBase [(successor, setInsert entry incoming)
                | successor <- successors block]
  where asBase = mkFactBase nodeset
        entry = entryLabel block
        incoming = getFact nodeset entry facts

-- analyzeCmmFwd :: DataflowLattice f -> TransferFun f -> CmmGraph -> FactBase f -> FactBase f

dominators :: node ~ CmmNode => GenCmmGraph node -> LabelMap DominatorSet
dominators g = 
  analyzeCmmFwd nodeset transfer g startFacts
      where startFacts = mkFactBase nodeset [(g_entry g, setSingleton (g_entry g))]
--            CmmGraph { g_graph = GMany NothingO blockmap NothingO } = g

