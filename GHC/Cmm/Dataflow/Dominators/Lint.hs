{-# LANGUAGE GADTs #-}

module GHC.Cmm.Dataflow.Dominators.Lint
  ( shortPaths'

  , dominatorsClimb 

  , dominatorsByPath
  , dominatorsByAnalysis
  , consistentDominators

  , dominatorsPassAllChecks
  )
where

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Dominators
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


-- | The literal definition: The dominators of node A are the ones that appear on 
-- every path ending in A.  The short paths suffice.  This one is slow to compute.

dominatorsByPath :: NonLocal node => GenCmmGraph node -> LabelMap LabelSet
dominatorsByPath g = foldl addPath mapEmpty (shortPaths' g)
  where addPath _ [] = panic "empty path"
        addPath map path@(final : _) = mapAlter (intersect (setFromList path)) final map

        intersect path Nothing = Just path
        intersect path (Just labels) = Just $ setIntersection path labels

-- | Dominators as computed by our analysis.

dominatorsByAnalysis :: NonLocal node => GenCmmGraph node -> LabelMap LabelSet
dominatorsByAnalysis g = mapMapWithKey domSet (gwd_dominators $ graphWithDominators g)
  where domSet lbl = collapse (setSingleton lbl)
        collapse _ AllNodes = panic "allnodes in dom set"
        collapse lbls EntryNode = lbls
        collapse lbls (NumberedNode _ l p) = collapse (setInsert l lbls) p


consistentDominators :: NonLocal node => GenCmmGraph node -> Bool
consistentDominators g = dominatorsByPath g == dominatorsByAnalysis g


dominatorsClimb :: NonLocal node => GenCmmGraph node -> Bool
dominatorsClimb g = mapAll decreasing (gwd_dominators $ result)
 where result = graphWithDominators g
       rpnum lbl = mapFindWithDefault (panic "missing") lbl (gwd_rpnumbering result)
       decreasing lbl = decreasingFrom (rpnum lbl)
       decreasingFrom _ AllNodes = panic "AllNodes"
       decreasingFrom _ EntryNode = True
       decreasingFrom k (NumberedNode n _ p) = n < k && decreasingFrom n p


mapAll :: IsMap m => (KeyOf m -> a -> Bool) -> m a -> Bool
mapAll goodPair = mapFoldlWithKey (\b k v -> b && goodPair k v) True


dominatorsPassAllChecks :: NonLocal node => GenCmmGraph node -> Bool
dominatorsPassAllChecks g = dominatorsClimb g && consistentDominators g
