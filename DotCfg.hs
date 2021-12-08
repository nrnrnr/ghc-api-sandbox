{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DotCfg (
  dotCFG
)
where

import Prelude hiding ((<>))
import GHC.Cmm.Dataflow.Dominators

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
            exitEdges $$
            text "exit [shape=\"plaintext\", label=" <>
                 doubleQuotes exitViz <> text "];" $$
            text "entry [shape=\"plaintext\", label=" <>
                 doubleQuotes entryviz <> irrcolor <> text "];" $$
            text "entry -> " <> dotName entry <> text ";")
    $$
    text "}"
  where edges = vcat $ map (dotEdge rpnum) $ mapFoldMapWithKey outedges blockmap
        nodes = vcat $ map (dotNode headers rpnum dmap) $ mapKeys blockmap
        outedges :: Label -> Block node C C -> [(Label, Label)]
        outedges label block = map (label,) $ successors block
        gwd = graphWithDominators g
        dmap :: LabelMap DominatorSet
        dmap = gwd_dominators gwd
        dominators lbl = getFact domlattice lbl dmap
        dominates lbl blockname = lbl == blockname || hasLbl (dominators blockname)
          where hasLbl AllNodes = False
                hasLbl EntryNode = False
                hasLbl (NumberedNode _ l p) = l == lbl || hasLbl p
        headers :: LabelSet
        headers = foldMap headersPointedTo blockmap
        headersPointedTo block =
            setFromList [label | label <- successors block,
                                          dominates label (entryLabel block)]
        rpnums = gwd_rpnumbering gwd
        rpnum lbl = mapFindWithDefault unreachableRPNum lbl rpnums

        entryviz = case reducibility rpnum dominates blockmap of
                     Reducible -> title
                     Irreducible -> title <> text "\\nIRREDUCIBLE"
        irrcolor = case reducibility rpnum dominates blockmap of
                     Reducible -> empty
                     Irreducible -> comma <> space <> text "color=" <>
                                    doubleQuotes (text "red")
        exitViz = text "exit:" <> space <> dotDominators exitDominators
        exitNodeLabels :: [Label]
        exitNodeLabels =
            foldMap (\n -> if null (successors n) then [entryLabel n] else []) blockmap
        exitDominators =
            foldMap (\lbl -> NumberedNode (rpnum lbl) lbl (dominators lbl)) exitNodeLabels
        exitEdges = vcat [dotName from <> text "-> exit;" | from <- exitNodeLabels]


data Reducibility = Reducible | Irreducible

reducibility :: NonLocal node
             => (Label -> RPNum)
             -> (Label -> Label -> Bool)
             -> LabelMap (Block node C C)
             -> Reducibility
reducibility rpnum dominates blockmap =
    if all goodBlock blockmap then Reducible else Irreducible
        where goodBlock b = unreachable b || all (goodEdge (entryLabel b)) (successors b)
              goodEdge from to = rpnum to > rpnum from || to `dominates` from
              unreachable b = rpnum (entryLabel b) == unreachableRPNum
dotNode :: LabelSet -> (Label -> RPNum) -> LabelMap DominatorSet -> Label -> SDoc
dotNode headers rpnum dmap label =
  dotName label <> space <>
  text "[label=" <> doubleQuotes dotlabel <> headermark <> text "]"
                <> text ";"
  where dotlabel = ppr label <> text "(" <> ppr nodenum <>
                   text "): " <> dotDominators (getFact domlattice label dmap)
        nodenum = rpnum label
        headermark = if setMember label headers then
                         space <> text "peripheries=2"
                     else
                         empty

dotDominators :: DominatorSet -> SDoc
dotDominators EntryNode = text "<entry>"
dotDominators AllNodes = text "<all>"
dotDominators (NumberedNode n _ parent) = ppr n <> text " -> " <> dotDominators parent


dotEdge :: (Label -> RPNum) -> (Label, Label) -> SDoc
dotEdge rpnum (from, to) = dotName from <> text "->" <> dotName to <> style <> text ";"
  where style = if rpnum to <= rpnum from then
                    space <> text "[color=\"blue\"]"
                else
                    empty

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
