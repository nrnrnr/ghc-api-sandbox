{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Data.Graph.Collapse
  ( collapseInductiveGraph
  , CollapseInfo(..)
  , VizCollapseMonad(..)
  , NullCollapseViz(..)
  , pprCollapseInfo
  )
where

{-|
Module      : GHC.Data.Graph.Collapse
Description : Implement the "collapsing" algorithm Hecht and Ullman

A control-flow graph is reducible if and only if it is collapsible
according to the definition of Hecht and Ullman (1972).   This module
implements the collapsing algorithm of Hecht and Ullman, and if it
encounters a graph that is not collapsible, it splits nodes until the
graph is fully collapsed.  It then reports what nodes (if any) had to
be split in order to collapse the graph.  The information is used
upstream to node-split Cmm graphs.

The module uses the inductive graph representation cloned from the
Functional Graph Library (Hackage package `fgl`, modules
`GHC.Data.Graph.Inductive.*`.)

-}

-- Full reference to paper: Matthew S. Hecht and Jeffrey D. Ullman
-- (1972).  Flow Graph Reducibility. SIAM J. Comput., 1(2), 188–202.
-- https://doi.org/10.1137/0201014


import Prelude hiding ((<>))

import Control.Exception
import Data.Function hiding ((&))
import Data.List

import GHC.Data.Graph.Inductive.Graph
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dominators (RPNum)
import GHC.Utils.Panic
import GHC.Utils.Outputable

-- | If you want to visualize the graph-collapsing algorithm, create
-- an instance of monad `VizCollapseMonad`.  Each step in the
-- algorithm is announced to the monad as a side effect.  If you don't
-- care about visualization, you would use the `NullCollapseViz`
-- monad, in which these operations are no-ops.

class (Graph gr, Monad m) => VizCollapseMonad m gr where
  consumeByInGraph :: Node -> Node -> gr CollapseInfo () -> m ()
  splitGraphAt :: gr CollapseInfo () -> LNode CollapseInfo -> m ()
  finalGraph :: gr CollapseInfo () -> m ()

-- | Use a final `CollapseInfo` record to find out what nodes, if
-- any, of an original graph needed to be split.  (A `CollapseInfo`
-- record describes a "supernode" that is the result of having merged
-- one or more "original nodes," called its /constituents/.)

data CollapseInfo =
    CollapseInfo { unsplitLabels :: LabelSet -- ^ Constituents that were never split
         , splitLabels :: LabelSet   -- ^ Constituents that were split
         , rpnumber :: RPNum         -- ^ Number of lowest constituent
         }


-- | Tell if a `Node` has a single predecessor.
singlePred :: Graph gr => gr a b -> Node -> Bool
singlePred gr n
    | ([_], _, _, _) <- context gr n = True
    | otherwise = False

-- | Use this function to extract information about a `Node` that you
-- know is in a `Graph`.  It's like `match` from `Graph`, but it must
-- succeed.
forceMatch :: Graph gr
           => Node -> gr CollapseInfo b -> (Context CollapseInfo b, gr CollapseInfo b)
forceMatch node g = case match node g of (Just c, g') -> (c, g')
                                         _ -> panicDump node g
 where panicDump :: Graph gr => Node -> gr CollapseInfo b -> any
       panicDump k _g =
        panic (showSDocUnsafe $
               text "/* failed to match node " <+> int k <+> text "*/"
               -- $$ dotGraph pprCollapseInfo (selected k) g
               -- for a more informative panic, import DotGraph and turn this on
              )


-- | Merge two nodes, return new graph plus list of nodes that newly have a single
-- predecessor.  This function implements transformation $T_2$ from
-- the Hecht and Ullman paper (merge the node into its unique
-- predecessor).  It then also removes self-edges (transformation $T_1$ from
-- the Hecht and Ullman paper).  There is no need for a separate
-- implementation of $T_1$.
--
-- `consumeBy v u g` returns the graph that results when node v is
-- consumed by node u in graph g.  Both v and u are replaced with a new node u'
-- with these properties:
--
--    LABELS(u') = LABELS(u) `union` LABELS(v)
--    SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
--    every node that previously points to u now points to u'
--
-- It also returns a list of nodes in the result graph that
-- are *newly* single-predecessor nodes.

consumeBy :: DynGraph gr
          => Node -> Node -> gr CollapseInfo () -> (gr CollapseInfo (), [Node])
consumeBy toNode fromNode g =
    assert (toPreds == [((), fromNode)]) $
    (newGraph, newCandidates)
  where ((toPreds,   _, to,   toSuccs),   g')  = forceMatch toNode   g
        ((fromPreds, _, from, fromSuccs), g'') = forceMatch fromNode g'
        info = from { unsplitLabels = unsplitLabels from `setUnion` unsplitLabels to
                    , splitLabels   = splitLabels   from `setUnion` splitLabels   to
                    , rpnumber = rpnumber from `min` rpnumber to
                    }
        context = ( fromPreds -- by construction, can't have `toNode`
                  , fromNode
                  , info
                  , delete ((), fromNode) toSuccs `union` fromSuccs
                  )
        newGraph = context & g''
        newCandidates = filter (singlePred newGraph) changedNodes
        changedNodes = fromNode `insert` map snd (toSuccs `intersect` fromSuccs)

-- | Split a given node.  The node is replaced with a collection of replicas,
-- one for each predecessor.  After the split, every predecessor
-- points to a unique replica.
split :: DynGraph gr => Node -> gr CollapseInfo b -> gr CollapseInfo b
split node g = assert (isMultiple preds) $ foldl addReplica g' newNodes
  where ((preds, _, info, succs), g') = forceMatch node g
        info' = info { unsplitLabels = setEmpty
                     , splitLabels = splitLabels info `setUnion` unsplitLabels info
                     }
        newNodes = zip preds [maxNode+1..]
        (_, maxNode) = nodeRange g
        addReplica g (pred, newNode) = ([pred], newNode, info', succs) & g


-- | Does a list have more than one element? (in constant time).
isMultiple :: [a] -> Bool
isMultiple [] = False
isMultiple [_] = False
isMultiple (_:_:_) = True

-- | Find a candidate for splitting by finding the first node (by
-- reverse postorder) that has multiple predecessors.
leastSplittable :: Graph gr => gr CollapseInfo () -> LNode CollapseInfo
leastSplittable g = lnode $ minimumBy (compare `on` num) splittable
  where splittable = filter (isMultiple . preds) $ map about $ labNodes g
        splittable :: [(Adj (), Node, RPNum, CollapseInfo)]
        preds (ps, _, _, _) = ps
        num (_, _, rp, _) = rp
        lnode (_, n, _, info) = (n, info)
        about :: (Node, CollapseInfo) -> (Adj (), Node, RPNum, CollapseInfo)
        about (n, info) = (ps, n, rpnumber info, info)
          where (ps, _, _, _) = context g n

-- | Test if a graph has but a single node.
singletonGraph :: Graph gr => gr a b -> Bool
singletonGraph g = case labNodes g of [_] -> True
                                      _ -> False

-- | Using the algorithm of Hecht and Ullman (1972), collapse a graph
-- into a single node, splitting nodes as needed.  Record
-- visualization events in monad `m`.
collapseInductiveGraph :: (DynGraph gr, VizCollapseMonad m gr)
                       => gr CollapseInfo () -> m (gr CollapseInfo ())
collapseInductiveGraph g = drain g worklist
  where worklist :: [[Node]] -- ^ nodes with exactly one predecessor
        worklist = [filter (singlePred g) $ nodes g]

        drain g [] = if singletonGraph g then finalGraph g >> return g
                     else let (n, info) = leastSplittable g
                          in  do splitGraphAt g (n, info)
                                 collapseInductiveGraph $ split n g
        drain g ([]:nss) = drain g nss
        drain g ((n:ns):nss) = let (g', ns') = consumeBy n (theUniquePred n) g
                               in  do consumeByInGraph n (theUniquePred n) g
                                      drain g' (ns':ns:nss)
           where theUniquePred n
                     | ([(_, p)], _, _, _) <- context g n = p
                     | otherwise =
                         panic "node claimed to have a unique predecessor; doesn't"

----------------------------------------------------------------

-- | Use this function to prettyprint diagnostics; it visualizes a
-- node labeled with `CollapseInfo`.
pprCollapseInfo :: LNode CollapseInfo -> SDoc
pprCollapseInfo (k, info) = int k <> ":" <+> "of" <+> text (show (rpnumber info))

----------------------------------------------------------------

-- | The identity monad as a `VizCollapseMonad`.  Use this monad when
-- you want efficiency in graph collapse.
newtype NullCollapseViz a = NullCollapseViz { unNCV :: a }

instance Graph gr => VizCollapseMonad NullCollapseViz gr where
  consumeByInGraph _ _ _ = return ()
  splitGraphAt _ _ = return ()
  finalGraph _ = return ()

instance Functor NullCollapseViz where
  fmap f (NullCollapseViz a) = NullCollapseViz (f a)

instance Applicative NullCollapseViz where
  pure = NullCollapseViz
  NullCollapseViz f <*> NullCollapseViz a = NullCollapseViz (f a)

instance Monad NullCollapseViz where
  return = pure
  NullCollapseViz a >>= k = k a
