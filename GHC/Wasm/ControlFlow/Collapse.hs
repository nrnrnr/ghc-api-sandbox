{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Wasm.ControlFlow.Collapse
--  ( structuredControl
--  )
where

import Control.Exception
import GHC.Data.Graph.Inductive
import GHC.Cmm.Dataflow.Dominators (RPNum)

--import Prelude hiding (succ)
--
import Data.Function hiding ((&))
import Data.List
--import Data.Maybe
--
--import GHC.Cmm
--import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
--import GHC.Cmm.Dataflow.Dominators
--import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
--import GHC.Cmm.Switch
--
--import GHC.Platform
--
import GHC.Utils.Panic
--import GHC.Utils.Outputable (Outputable, text, (<+>), ppr
--                            , showSDocUnsafe
--                            , pprWithCommas
--                            )
--
--

data Info = I { unsplitLabels :: LabelSet
              , splitLabels :: LabelSet
              , rpnumber :: RPNum
              }

mapMinus :: IsMap map => map a -> KeyOf map -> map a
mapMinus = flip mapDelete

setMap :: IsSet set => (ElemOf set -> ElemOf set) -> set -> set
setMap f = setFoldl (\mapped a -> setInsert (f a) mapped) setEmpty

isSingleton :: IsSet set => set -> Bool
isSingleton s = case setElems s of [_] -> True
                                   _ -> False

{-
consumeBy toL fromL g =
  assert (isSingleton $ setMap meaning $ preds to) $
  assert (not $ mapMember toL (aliases g)) $
  error "unfinished" from' nodeMap' aliases'
 where to = node toL
       from = node fromL
       node label = fromJust $ mapLookup (meaning label) (nodeMap g)
       meaning label = mapFindWithDefault label label (aliases g)
       from' = from { preds = setDelete toL (preds from)
                    , unsplitLabels = unsplitLabels from `setUnion` unsplitLabels to
                    , splitLabels   = splitLabels   from `setUnion` splitLabels   to
                    , rpnumber = rpnumber from `min` rpnumber to
                    }
       nodeMap' = nodeMap g `mapMinus` fromL `mapMinus` toL
       aliases' = mapInsert toL fromL $ aliases g
-}


forceMatch :: Graph gr => Node -> gr a b -> (Context a b, gr a b)
forceMatch node g = case match node g of (Just c, g') -> (c, g')
                                         _ -> panic "missing node"

consumeBy :: DynGraph gr => Node -> Node -> gr Info () -> gr Info ()
consumeBy toNode fromNode g =
    assert (toPreds == [((), fromNode)]) $
    context & g''    
  where ((toPreds, _, to, toSuccs), g') = forceMatch toNode g
        ((fromPreds, _, from, fromSuccs), g'') = forceMatch toNode g'
        info = from { unsplitLabels = unsplitLabels from `setUnion` unsplitLabels to
                    , splitLabels   = splitLabels   from `setUnion` splitLabels   to
                    , rpnumber = rpnumber from `min` rpnumber to
                    }
        context = ( fromPreds
                  , fromNode
                  , info
                  , delete ((), fromNode) toSuccs ++ fromSuccs
                  )
    
--consumeBy :: Label -> Label -> Graph -> Graph
   -- ^ `consumeBy v u` returns the graph that results when node v is
   -- consumed by node u.  Both v and u are replaced with a new node u' 
   -- with these properties:
   --    LABELS(u') = LABELS(u) `union` LABELS(v)
   --    SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
   --    every node that previously points to u now points to u'
   -- 

split :: DynGraph gr => Node -> gr Info b -> gr Info b
split node g = assert (isMultiple preds) $ foldl addReplica g' newNodes
  where ((preds, _, info, succs), g') = forceMatch node g
        info' = info { unsplitLabels = setEmpty
                     , splitLabels = splitLabels info `setUnion` unsplitLabels info
                     }
        newNodes = zip preds [maxNode+1..]
        (_, maxNode) = nodeRange g
        addReplica g (pred, node) = ([pred], node, info', succs) & g

                                  
isMultiple :: [a] -> Bool
isMultiple [] = False
isMultiple [_] = False
isMultiple (_:_:_) = True

consumeableEdge :: Graph gr => gr a b -> Maybe Edge
hasExactlyOneNode :: Graph gr => gr a b -> Bool
leastSplittable :: Graph gr => gr Info () -> Node

hasExactlyOneNode g = case labNodes g of [_] -> True
                                         _ -> False
consumeableEdge = error "unimp"

leastSplittable g = node $ minimumBy (compare `on` num) splittable
  where splittable = filter (isMultiple . preds) $ map about $ labNodes g
        preds (ps, _, _) = ps
        num (_, _, rp) = rp
        node (_, n, _) = n
        about :: (Node, Info) -> (Adj (), Node, RPNum)
        about (n, info) = (ps, n, rpnumber info)
          where (ps, _, _, _) = context g n

collapse :: DynGraph gr => gr Info () -> gr Info ()
collapse g = if hasExactlyOneNode g then g else collapse (next g)
  where next g = case consumeableEdge g of
                   Just (u, v) -> consumeBy v u g
                   Nothing -> split (leastSplittable g) g

{- 

I've implemented an algorithm on a directed graph that basically goes like this:

  1. Find a node with exactly one predecessor
  2. Remove that node, transferring its outgoing edges to the unique predecessor
  3. Repeat until no single-predecessor nodes are left

I'm struggling to think of an efficient way to implement step 1.  Right now I'm using linear search, which makes the cost of the whole algorithm quadratic.  Not good.  I'm quite willing to use an auxiliary data structure, but I haven't been smart enough to think of one that I can update efficiently as nodes are removed and edges are transferred.

Does anybody have any ideas?

-}


{-

I'm advertising for programming advice.  I need to implement an
algorithm on a directed graph, but my purely functional data
structures are rusty, and I'm having trouble thinking of a
representation that will support the operations I want.  My top
criterion is that the code be clean, but it's also necessary that the
efficiency not embarrass anyone.

I'd love to schedule a call or maybe do some pair programming.

Problem details in a reply.


I'd like to implement an algorithm on the following species of directed graph:

  - Every node `u` carries a set of labels `LABELS(u)`.
  - Every node `u` carries a number `RPNUM(u)`.
  - Every node `u` has a *set* of successors `SUCC(u)`. (A node's predecessors likewise form a *set*.)
  - A node is never its own successor; that is `¬∃u : u ∈ SUCC(u)`.

My purely functional data structures are rusty, and I'm having trouble thinking of a representation that will support the operations I want.  My top criterion is that the code be clean, but it's also necessary that the efficiency not embarrass anyone.

The key operation I need is *consumption* of one node by another:

 1. When `u` points to `v` and `u` is the *only* node that points to `v` (that is, `u` is the unique predecessor of `v`, `u` *consumes* `v`.  This is basically a merge operation.  Let us say functionally that `u` is replaced by `u'` such that
    
      - In the new graph, LABELS(u') = LABELS(u) ∪ LABELS(v)
      - SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
      - RPNUM(u') = RPNUM(u) `min` RPNUM(v)
      - Every node that formerly pointed to `u` now points to `u'`

In addition, I need to be able to implement these other operations:

 2. Find a node with a unique predecessor, if one exists.  That is, find a pair `(u, v)` such that `u` can consume `v`.

 3. Say if the graph contains more than one node.

 4. If there is a node that has multiple predecessors, find the one with the smallest `RPNUM`.

 5. *Split* a node that has multiple predecessors.  That means make one copy for each predecessor.  In the new graph, each predecessor points to its unique copy.  Each copy has the same `LABELS`, `SUCC`, and `RPNUM` as the original.

-}

