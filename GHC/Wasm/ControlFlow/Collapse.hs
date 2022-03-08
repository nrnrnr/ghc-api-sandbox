{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Wasm.ControlFlow.Collapse
  ( collapse
  , Info(..)
  , VizCollapseMonad(..)
  , infoViz
  )
where

import Prelude hiding ((<>))

import DotGraph
import Control.Exception
import GHC.Data.Graph.Inductive.Graph
import GHC.Cmm.Dominators (RPNum)

import Data.Function hiding ((&))
import Data.List
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Panic
import GHC.Utils.Outputable

class (Graph gr, Monad m) => VizCollapseMonad m gr where
  consumeByInGraph :: Node -> Node -> gr Info () -> m ()
  splitGraphAt :: gr Info () -> LNode Info -> m ()
  finalGraph :: gr Info () -> m ()

-- | An `Info` record describes a "supernode" that is the result of
-- having merged one or more "original nodes," called its /constituents/.
--
data Info =
    Info { unsplitLabels :: LabelSet -- ^ Constituents that were never split
         , splitLabels :: LabelSet   -- ^ Constituents that were split
         , rpnumber :: RPNum         -- ^ Number of lowest constituent
         }

-- | Node has a single predecessor.
singlePred :: Graph gr => gr a b -> Node -> Bool
singlePred gr n
    | ([_], _, _, _) <- context gr n = True
    | otherwise = False

-- | Like `match` from Graph, but must succeed.
forceMatch :: Graph gr => Node -> gr Info b -> (Context Info b, gr Info b)
forceMatch node g = case match node g of (Just c, g') -> (c, g')
                                         _ -> panicDump node g

panicDump :: Graph gr => Node -> gr Info b -> any
panicDump k g =
  panic (showSDocUnsafe $
         text "/* failed to match node " <+> int k <+> text "*/"  $$
              dotGraph infoViz (selected k) g)


-- | Merge two nodes, return new graph plus list of nodes that newly have a single
-- predecessor
--
-- `consumeBy v u g` returns the graph that results when node v is
-- consumed by node u in graph g.  Both v and u are replaced with a new node u'
-- with these properties:
--    LABELS(u') = LABELS(u) `union` LABELS(v)
--    SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
--    every node that previously points to u now points to u'
-- It also returns a list of nodes in the result graph that
-- are *newly* single-predecessor nodes.

consumeBy :: DynGraph gr => Node -> Node -> gr Info () -> (gr Info (), [Node])
consumeBy toNode fromNode g =
    assert (toPreds == [((), fromNode)]) $
    (newGraph, newCandidates)
  where ((toPreds,   _, to,   toSuccs),  g')   = forceMatch toNode   g
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

-- | Split a given node.
split :: DynGraph gr => Node -> gr Info b -> gr Info b
split node g = assert (isMultiple preds) $ foldl addReplica g' newNodes
  where ((preds, _, info, succs), g') = forceMatch node g
        info' = info { unsplitLabels = setEmpty
                     , splitLabels = splitLabels info `setUnion` unsplitLabels info
                     }
        newNodes = zip preds [maxNode+1..]
        (_, maxNode) = nodeRange g
        addReplica g (pred, node) = ([pred], node, info', succs) & g


-- | Does a list have more than one element, in constant time.
isMultiple :: [a] -> Bool
isMultiple [] = False
isMultiple [_] = False
isMultiple (_:_:_) = True

-- | Find a candidate for splitting by finding the first node that has
-- multiple predecessors.
leastSplittable :: Graph gr => gr Info () -> LNode Info
leastSplittable g = lnode $ minimumBy (compare `on` num) splittable
  where splittable = filter (isMultiple . preds) $ map about $ labNodes g
        splittable :: [(Adj (), Node, RPNum, Info)]
        preds (ps, _, _, _) = ps
        num (_, _, rp, _) = rp
        lnode (_, n, _, info) = (n, info)
        about :: (Node, Info) -> (Adj (), Node, RPNum, Info)
        about (n, info) = (ps, n, rpnumber info, info)
          where (ps, _, _, _) = context g n

singletonGraph :: Graph gr => gr a b -> Bool
singletonGraph g = case labNodes g of [_] -> True
                                      _ -> False

-- | Using the algorithm of Hecht and Ullman (1972), collapse a graph
-- into a single node, splitting nodes as needed.
collapse :: (DynGraph gr, VizCollapseMonad m gr) => gr Info () -> m (gr Info ())
collapse g = drain g worklist
  where worklist :: [[Node]] -- ^ nodes with exactly one predecessor
        worklist = [filter (singlePred g) $ nodes g]

        drain g [] = do if singletonGraph g then finalGraph g >> return g
                        else let (n, info) = leastSplittable g
                             in  do splitGraphAt g (n, info)
                                    collapse $ split n g
        drain g ([]:nss) = drain g nss
        drain g ((n:ns):nss) = let (g', ns') = consumeBy n (theUniquePred n) g
                               in  do consumeByInGraph n (theUniquePred n) g
                                      drain g' (ns':ns:nss)
           where theUniquePred n
                     | ([(_, p)], _, _, _) <- context g n = p
                     | otherwise = panic "node claimed to have a unique predecessor; doesn't"

infoViz :: LNode Info -> SDoc
infoViz (k, info) = int k <> ":" <+> "of" <+> text (show (rpnumber info))
