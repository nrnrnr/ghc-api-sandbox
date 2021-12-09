{-# LANGUAGE GADTs #-}

module GHC.Cmm.Dataflow.Dominators.Lint
where

--import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
--import GHC.Cmm.Dataflow.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm 

import GHC.Utils.Panic

-- | (Reversed) path through a CmmGraph, last node first.
-- For example, in { A(); if (p) { B(); } else { C(); }; D(); },
-- one path is A -> C -> D, and it would be represented as
-- [D, C, A].

type Path' = [Label]

shortPaths' :: NonLocal node => GenCmmGraph node ->  [Path']
shortPaths' g = pathsPrefixed (g_entry g) [] setEmpty
  where pathsPrefixed :: Label -> Path' -> LabelSet -> [Path']
            -- ^ returns a list of all _short_ paths that begin with (block : prefix),
            -- where a short path is one that contains at most one repeated label,
            -- which must be the last one on the path (and so at the head of the list).
            -- Precondition: `visited == setFromList prefix`.
        pathsPrefixed lbl prefix visited = toLbl : extensions
          where toLbl = lbl : prefix
                visited' = setInsert lbl visited
                extensions = if setMember lbl visited then []
                             else concat [pathsPrefixed s toLbl visited' |
                                            s <- successors (blockLabeled lbl)]

        blockLabeled lbl = mapFindWithDefault (panic "missing block") lbl blockmap


        CmmGraph { g_graph = GMany NothingO blockmap NothingO } = g
