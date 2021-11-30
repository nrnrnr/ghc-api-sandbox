{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DotCfg (
  dotCFG  
)
where

import Data.List
import Prelude hiding ((<>))
import FastDom

import GHC.Cmm
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable


                     


dotCFG :: forall node . (NonLocal node) => SDoc -> GenCmmGraph node -> SDoc
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
        nodes = vcat $ map(dotNode headers rpnums dmap) $ mapKeys blockmap
        outedges :: Label -> Block node C C -> [(Label, Label)]
        outedges label block = map (label,) $ successors block
        dmap :: LabelMap DominatorSet
        dmap = dominatorMap g
        dominators lbl = getFact domlattice lbl dmap
        dominates lbl blockname = has (rpnum lbl) (dominators blockname)
            where has _ AllNodes = False
                  has _ EntryNode = False
                  has n (NumberedNode m p) = m == n || has n p
        headers :: LabelSet
        headers = foldMap headersPointedTo blockmap
        headersPointedTo block =
            setFromList [label | label <- successors block,
                                          dominates label (entryLabel block)]
        rpnums = rpmap g
        rpnum lbl = mapFindWithDefault 0 lbl rpnums
            

dotNode :: LabelSet -> LabelMap Int -> LabelMap DominatorSet -> Label -> SDoc
dotNode headers rpmap dmap label =
  dotName label <> space <>
  text "[label=" <> doubleQuotes dotlabel <> headermark <> text "]"
                <> text ";"
  where dotlabel = dotName label <> text "(" <> int nodenum <> 
                   text "): " <> dotDominators (getFact domlattice label dmap)
        nodenum = mapFindWithDefault 0 label rpmap
        headermark = if setMember label headers then
                         space <> text "peripheries=2"
                     else
                         empty

dotDominators :: DominatorSet -> SDoc
dotDominators EntryNode = text "<entry>"
dotDominators AllNodes = text "<all>"
dotDominators (NumberedNode n parent) = int n <> text " -> " <> dotDominators parent


dotEdge :: (Label, Label) -> SDoc
dotEdge (from, to) = dotName from <> text "->" <> dotName to <> text ";"

dotName :: Label -> SDoc
dotName label = text ("L" ++ shortdigits)
  where shortdigits = case show label of
                        'L' : digits -> show ((read digits :: Int) `mod` dmod)
                        urk -> urk
        dmod = 10 ^ ndigits


ndigits :: Int -- ^ Number of digits of a label to show
ndigits = 3

--targets :: Block CmmNode e C -> [Label]
----targets _ = [mkHooplLabel 99]
--targets (CmmBranch target) = [target]
--targets (CmmCondBranch { cml_true = t, cml_false = f }) = [t, f]
--targets (CmmSwitch _ ts) = panic "switch targets not implemented"
--targets (CmmCall { cml_cont = k }) = toList k
--targets (CmmForeignCall { succ = l }) = [l]
