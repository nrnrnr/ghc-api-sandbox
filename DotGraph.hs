{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DotGraph
  ( dotGraph
  )
where

import Prelude hiding ((<>))

import GHC.Utils.Outputable
--import GHC.Utils.Panic

import GHC.Cmm.Dataflow.Label
import GHC.Data.Graph.Inductive
import GHC.Wasm.ControlFlow.Collapse(Info(..))


dotGraph :: Graph gr => (Label -> SDoc) -> Maybe Node -> gr Info b -> SDoc
dotGraph = undefined

{-
dotCFG nodeTag title (g@CmmGraph { g_graph = GMany NothingO blockmap NothingO, g_entry = entry }) =
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
        nodes = vcat $ map (dotNode nodeTag headers rpnum dmap) $ mapToList blockmap
        outedges :: Label -> Block node C C -> [(Label, Label)]
        outedges label block = map (label,) $ successors block
        gwd = graphWithDominators g
        dmap :: LabelMap DominatorSet
        dmap = gwd_dominators gwd
        dominators lbl = mapLookup lbl dmap
        dominates lbl blockname = lbl == blockname || hasLbl (dominators blockname)
          where hasLbl (Just EntryNode) = False
                hasLbl (Just (ImmediateDominator l p)) = l == lbl || hasLbl (Just p)
                hasLbl Nothing = False
        headers :: LabelSet
        headers = foldMap headersPointedTo blockmap
        headersPointedTo block =
            setFromList [label | label <- successors block,
                                          dominates label (entryLabel block)]
        rpnums = gwd_rpnumbering gwd
        rpnum lbl = mapFindWithDefault unreachableRPNum lbl rpnums

        entryviz = case fastReducibility rpnum dominates blockmap of
                     Reducible -> title
                     Irreducible -> title <> text "\\nIRREDUCIBLE"
        irrcolor = case fastReducibility rpnum dominates blockmap of
                     Reducible -> empty
                     Irreducible -> comma <> space <> text "color=" <>
                                    doubleQuotes (text "red")
        exitViz = text "exit:" <+> dotDominators (Just exitDominators)
        exitNodeLabels :: [Label]
        exitNodeLabels =
            foldMap (\n -> if null (successors n) then [entryLabel n] else []) blockmap
        exitDominators = foldl1 intersectDominatorsSlow $ map addDoms exitNodeLabels
            where addDoms lbl = ImmediateDominator lbl $
                                case dominators lbl of Nothing -> EntryNode
                                                       Just doms -> doms
        exitEdges = vcat [dotName from <> text "-> exit;" | from <- exitNodeLabels]


data Reducibility = Reducible | Irreducible
  deriving (Eq, Show)

reducibility :: (NonLocal node) => GraphWithDominators node -> Reducibility
reducibility gwd = fastReducibility rpnum dominates (graphMap $ gwd_graph gwd)
  where rpnums = gwd_rpnumbering gwd
        rpnum lbl = mapFindWithDefault unreachableRPNum lbl rpnums

        dmap = gwd_dominators gwd
        dominators lbl = mapFindWithDefault (panic ("reducibility: no dominator for " ++ showPprUnsafe lbl)) lbl dmap
        dominates lbl blockname = lbl == blockname || hasLbl (dominators blockname)
          where hasLbl EntryNode = False
                hasLbl (ImmediateDominator l p) = l == lbl || hasLbl p


fastReducibility :: NonLocal node
             => (Label -> RPNum)
             -> (Label -> Label -> Bool)
             -> LabelMap (Block node C C)
             -> Reducibility
fastReducibility rpnum dominates blockmap =
    if all goodBlock blockmap then Reducible else Irreducible
        where goodBlock b = unreachable b || all (goodEdge (entryLabel b)) (successors b)
              goodEdge from to = rpnum to > rpnum from || to `dominates` from
              unreachable b = rpnum (entryLabel b) == unreachableRPNum
dotNode :: (a -> SDoc) -> LabelSet -> (Label -> RPNum) -> LabelMap DominatorSet -> (Label, a) -> SDoc
dotNode display headers rpnum dmap (label, a) =
  dotName label <> space <>
  text "[label=" <> doubleQuotes (hcat [display a, dotlabel]) <> headermark <> text "]"
                <> text ";"
  where dotlabel =
            if False then -- noisy
                text " ==" <+> ppr label <> text "(" <> ppr nodenum <>
                text "): " <> dotDominators (mapLookup label dmap)
            else
                empty
        nodenum = rpnum label
        headermark = if setMember label headers then
                         space <> text "peripheries=2"
                     else
                         empty

dotDominators :: Maybe DominatorSet -> SDoc
dotDominators (Just EntryNode) = text "<entry>"
dotDominators (Just (ImmediateDominator lbl parent)) = ppr lbl <> text " -> " <> dotDominators (Just parent)
dotDominators Nothing = text "<unreachable?>"


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
-}
