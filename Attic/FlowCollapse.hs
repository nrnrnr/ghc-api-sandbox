{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Attic.FlowCollapse
  ( collapse
  , Info(..)
  , VizMonad(..)
  , infoViz
  )
where

import Prelude hiding ((<>))

import DotGraph
import Control.Exception
import GHC.Data.Graph.Inductive.Graph
import GHC.Cmm.Dominators (RPNum)

--import Prelude hiding (succ)
--
import Data.Function hiding ((&))
import Data.List
--import Data.Maybe
--
--import GHC.Cmm
--import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
--import GHC.Cmm.Dominators
--import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
--import GHC.Cmm.Switch
--
--import GHC.Platform
--
import GHC.Utils.Panic
import GHC.Utils.Outputable -- (Outputable, text, (<+>), ppr
--                            , showSDocUnsafe
--                            , pprWithCommas
--                            )
--
--
--import System.IO.Unsafe
--import System.IO
--import Debug.Trace

trace :: String -> b -> b
trace _ b = b

class (Graph gr, Monad m) => VizMonad m gr where
  consumeByInGraph :: Node -> Node -> gr Info () -> m ()
  splitGraphAt :: gr Info () -> LNode Info -> m ()
  finalGraph :: gr Info () -> m ()



data Info = Info { unsplitLabels :: LabelSet
                 , splitLabels :: LabelSet
                 , rpnumber :: RPNum
                 }

-- mapMinus :: IsMap map => map a -> KeyOf map -> map a
-- mapMinus = flip mapDelete
--
-- setMap :: IsSet set => (ElemOf set -> ElemOf set) -> set -> set
-- setMap f = setFoldl (\mapped a -> setInsert (f a) mapped) setEmpty

-- isSingleton :: IsSet set => set -> Bool
-- isSingleton s = case setElems s of [_] -> True
--                                   _ -> False

singlePred :: Graph gr => gr a b -> Node -> Bool
singlePred gr n
    | ([_], _, _, _) <- context gr n = True
    | otherwise = False

forceMatch :: Graph gr => Node -> gr Info b -> (Context Info b, gr Info b)
forceMatch node g = case match node g of (Just c, g') -> (c, g')
                                         _ -> panicDump node g

panicDump :: Graph gr => Node -> gr Info b -> any
panicDump k g =
  trace (showSDocUnsafe $
         text "/* matching node " <+> int k <+> text "*/"  $$
              dotGraph infoViz (selected k) g) $
  panic "missing node, really"



-- | Merge two nodes, return new graph plus list of nodes that newly have a single
-- predecessor

   -- ^ `consumeBy v u g` returns the graph that results when node v is
   -- consumed by node u in graph g.  Both v and u are replaced with a new node u'
   -- with these properties:
   --    LABELS(u') = LABELS(u) `union` LABELS(v)
   --    SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
   --    every node that previously points to u now points to u'
   -- It also returns a list of nodes in the result graph that
   -- are *newly* single-predecessor nodes.

consumeBy :: DynGraph gr => Node -> Node -> gr Info () -> (gr Info (), [Node])
consumeBy toNode fromNode g =
    traceDoc ("/* target" <+> int toNode <+> "source" <+> int fromNode <+> "*/" $$
              dotGraph infoViz (selected toNode) g) $
    assert (toPreds == [((), fromNode)]) $
    (traceDoc ("/* new graph */" $$ vizGraph newGraph) newGraph,
     trace ("/* new candidates " ++ show newCandidates ++ " */") newCandidates)
  where ((toPreds, _, to, toSuccs), g') = forceMatch toNode g
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

{-
consumeBy' :: DynGraph gr => Node -> Node -> gr Info () -> gr Info ()
consumeBy' toNode fromNode g =
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
-}

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

--consumeableEdge :: Graph gr => gr a b -> Maybe Edge
-- hasExactlyOneNode :: Graph gr => gr a b -> Bool
leastSplittable :: Graph gr => gr Info () -> LNode Info

leastSplittable g = traceDoc ("/* search for least splittable */" $$ vizGraph g) $
                    lnode $ minimumBy (compare `on` num) $
                    trace ("/* splittable == " ++ show (map num splittable) ++ " */") splittable
  where splittable = filter (isMultiple . preds) $ map about $ labNodes g
        splittable :: [(Adj (), Node, RPNum, Info)]
        preds (ps, _, _, _) = ps
        num (_, _, rp, _) = rp
        lnode (_, n, _, info) = (n, info)
        about :: (Node, Info) -> (Adj (), Node, RPNum, Info)
        about (n, info) = (ps, n, rpnumber info, info)
          where (ps, _, _, _) = context g n

--preds :: Graph gr => gr a b -> Node -> [Node]
--preds g n = map snd ps
--    where (ps, _, _, _) = context g n

singletonGraph :: Graph gr => gr a b -> Bool
singletonGraph g = case labNodes g of [_] -> True
                                      _ -> False


collapse :: (DynGraph gr, VizMonad m gr) => gr Info () -> m (gr Info ())
collapse g = drain' g $ trace ("/* worklist is " ++ show worklist ++ " */") worklist
  where worklist :: [[Node]] -- ^ nodes with exactly one predecessor
        worklist = [filter (singlePred g) $ nodes g]

        drain' g work = trace ("/* draining graph, worklist " ++ show (concat work) ++ " */\n" ++
                          showSDocUnsafe (vizGraph g)) $ drain g work

        drain g [] = do if singletonGraph g then finalGraph g >> return g
                        else let (n, info) = leastSplittable g
                             in  do splitGraphAt g (n, info)
                                    collapse $ split n g
        drain g ([]:nss) = drain g nss
        drain g ((n:ns):nss) = trace ("/* absorb " ++ show n ++ " */") $
                               let (g', ns') = consumeBy n (theUniquePred n) g
                               in  do consumeByInGraph n (theUniquePred n) g
                                      drain' g' (ns':ns:nss)
           where theUniquePred n
                     | ([(_, p)], _, _, _) <- context g n = p
                     | otherwise = panic "node claimed to have a unique predecessor; doesn't"



-- singleton :: [a] -> Bool
-- singleton [_] = True
-- singleton _ = False

{-

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


traceDoc :: SDoc -> a -> a
traceDoc doc a = trace (showSDocUnsafe doc) a

infoViz :: LNode Info -> SDoc
infoViz (k, info) = int k <> ":" <+> "of" <+> text (show (rpnumber info))

vizGraph :: Graph gr => gr Info b -> SDoc
vizGraph = dotGraph infoViz noSelection
