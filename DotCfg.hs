{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DotCfg (
  dotCFG  
)
where

import Prelude hiding ((<>))

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable
import GHC.Cmm.Dataflow.Collections (IsMap(mapFoldMapWithKey, mapKeys))

                     

dotCFG :: forall node . NonLocal node => GenCmmGraph node -> SDoc
dotCFG (CmmGraph { g_graph = GMany NothingO blockmap NothingO, g_entry = entry }) =
    -- blockmap is foldable and traversable
    text "digraph {" $$
    nest 2 (edges $$
            nodes $$
            text "entry [shape=\"plaintext\", label=\"entry\"];" $$
            text "entry -> " <> dotName entry <> text ";")
    $$
    text "}"
  where edges = vcat $ map dotEdge $ mapFoldMapWithKey outedges blockmap
        nodes = vcat $ map dotNode $ mapKeys blockmap
        outedges :: Label -> Block node C C -> [(Label, Label)]
        outedges label block = map (label,) $ successors block

dotNode :: Label -> SDoc
dotNode label =
  text "node [label=" <> doubleQuotes (dotName label) <> space <> text "]"
                <> space <> doubleQuotes (dotName label)
                <> text ";"

dotEdge :: (Label, Label) -> SDoc
dotEdge (from, to) = dotName from <> text "->" <> dotName to <> text ";"

dotName :: Label -> SDoc
dotName label = text $ show label -- hoping for nonnegative here


--targets :: Block CmmNode e C -> [Label]
----targets _ = [mkHooplLabel 99]
--targets (CmmBranch target) = [target]
--targets (CmmCondBranch { cml_true = t, cml_false = f }) = [t, f]
--targets (CmmSwitch _ ts) = panic "switch targets not implemented"
--targets (CmmCall { cml_cont = k }) = toList k
--targets (CmmForeignCall { succ = l }) = [l]
