{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DotGraph
  ( dotGraph
  , noSelection
  , selected
  )
where

import Prelude hiding ((<>))

import Data.Maybe

import GHC.Utils.Outputable
import GHC.Data.Graph.Inductive.Graph hiding (empty)

type Properties = [(String, SDoc)]

noSelection :: [(Node, Properties)]
noSelection = []

selected :: Node -> [(Node, Properties)]
selected k = [(k, [("peripheries", int 2)])]

dotGraph :: Graph gr => (LNode a -> SDoc) -> [(Node, Properties)] -> gr a b -> SDoc
dotGraph vizLabel selection graph = "digraph {" $$ nest 2 (nodes $$ edges) $$ "}"
  where edges = vcat $ map edge $ labEdges graph
        nodes = vcat $ map node $ labNodes graph
        node n@(k, _) =
            nodename k <+> attributes (label (vizLabel n) (props k)) <> ";"
        edge (from, to, _) = nodename from <+> "->" <+> nodename to
        nodename k = "N" <> int k
        props k = fromMaybe [] $ lookup k selection
        label l props = if any ((== "label") . fst) props then
                            props
                        else
                            ("label", l) : props

attributes :: [(String, SDoc)] -> SDoc
attributes [] = empty
attributes as = brackets $ pprWithCommas pair as
  where pair (label, doc) = text label <> "=" <> doubleQuotes doc
