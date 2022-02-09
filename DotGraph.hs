{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DotGraph
  ( dotGraph
  )
where

import Prelude hiding ((<>))

import GHC.Utils.Outputable
import GHC.Data.Graph.Inductive hiding (empty)


dotGraph :: Graph gr => (LNode a -> SDoc) -> Maybe Node -> gr a b -> SDoc
dotGraph vizLabel selection graph = "digraph {" $$ nest 2 (nodes $$ edges) $$ "}"
  where edges = vcat $ map edge $ labEdges graph
        nodes = vcat $ map node $ labNodes graph
        node n@(k, _) =
            nodename k <+> attributes (maybeSelected k [("label", vizLabel n)]) <> ";"
        edge (from, to, _) = nodename from <+> "->" <+> nodename to
        nodename k = "N" <> int k
        maybeSelected k pairs
            | selection == Just k = ("peripheries", int 2) : pairs
            | otherwise = pairs

attributes :: [(String, SDoc)] -> SDoc
attributes [] = empty
attributes as = brackets $ pprWithCommas pair as
  where pair (label, doc) = text label <> "=" <> doubleQuotes doc
