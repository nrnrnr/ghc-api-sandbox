module GHC.Data.Graph.Inductive 
  (
    -- * General Type Defintions
    -- ** Node and Edge Types
    Node, LNode,
    Edge,
    -- ** Types Supporting Inductive Graph View
    Adj,Context,MContext,Decomp,GDecomp, -- ,UContext,UDecomp,
    --Path,LPath(..),UPath,
     -- * Graph Type Classes
    -- | We define two graph classes:
    --
    --   Graph: static, decomposable graphs.
    --    Static means that a graph itself cannot be changed
    --
    --   DynGraph: dynamic, extensible graphs.
    --             Dynamic graphs inherit all operations from static graphs
    --             but also offer operations to extend and change graphs.
    --
    -- Each class contains in addition to its essential operations those
    -- derived operations that might be overwritten by a more efficient
    -- implementation in an instance definition.
    --
    -- Note that labNodes is essentially needed because the default definition
    -- for matchAny is based on it: we need some node from the graph to define
    -- matchAny in terms of match. Alternatively, we could have made matchAny
    -- essential and have labNodes defined in terms of ufold and matchAny.
    -- However, in general, labNodes seems to be (at least) as easy to define
    -- as matchAny. We have chosen labNodes instead of the function nodes since
    -- nodes can be easily derived from labNodes, but not vice versa.
    Graph(..),
    DynGraph(..),
    ufold,
    context
  )
where

import Data.Maybe

type Node = Int

-- | Labeled node
type LNode a = (Node,a)

-- | Unlabeled edge
type  Edge   = (Node,Node)
-- | Labeled edge
type LEdge b = (Node,Node,b)

--   In other words, this captures all information regarding the
--   specified 'Node' within a graph.
type Context a b  = (Adj b,Node,a,Adj b) -- Context a b "=" Context' a b "+" Node
type MContext a b = Maybe (Context a b)
-- | 'Graph' decomposition - the context removed from a 'Graph', and the rest
-- of the 'Graph'.
type Decomp g a b = (MContext a b,g a b)
-- | The same as 'Decomp', only more sure of itself.
type GDecomp g a b  = (Context a b,g a b)

-- | Labeled links to or from a 'Node'.
type Adj b        = [(b,Node)]


-- | Minimum implementation: 'empty', 'isEmpty', 'match', 'mkGraph', 'labNodes'
class Graph gr where
  {-# MINIMAL empty, isEmpty, match, mkGraph, labNodes #-}

  -- | An empty 'Graph'.
  empty     :: gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match     :: Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  --   For graphs that are also instances of 'DynGraph', @mkGraph ns
  --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  --   'empty'@.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]

  -- | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  -- and the remaining 'Graph'.
  matchAny  :: gr a b -> GDecomp gr a b
  matchAny g = case labNodes g of
                 (v,_):_ | (Just c,g') <- match v g -> (c,g')
                 _ -> error "Match Exception, Empty Graph"
                     

  -- | The number of 'Node's in a 'Graph'.
  noNodes   :: gr a b -> Int
  noNodes = length . labNodes

  -- | The minimum and maximum 'Node' in a 'Graph'.
  nodeRange :: gr a b -> (Node,Node)
  nodeRange g
    | isEmpty g = error "nodeRange of empty graph"
    | otherwise = (minimum vs, maximum vs)
    where
      vs = nodes g

  -- | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: gr a b -> [LEdge b]
  labEdges = ufold (\(_,v,_,s)->(map (\(l,w)->(v,w,l)) s ++)) []

-- | Fold a function over the graph by recursively calling 'match'.
ufold :: (Graph gr) => (Context a b -> c -> c) -> c -> gr a b -> c
ufold f u g
  | isEmpty g = u
  | otherwise = f c (ufold f u g')
  where
    (c,g') = matchAny g

class (Graph gr) => DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  --
  --   Context adjacencies should only refer to either a Node already
  --   in a graph or the node in the Context itself (for loops).
  --
  --   Behaviour is undefined if the specified 'Node' already exists
  --   in the graph.
  (&) :: Context a b -> gr a b -> gr a b


-- | List all 'Node's in the 'Graph'.
nodes :: (Graph gr) => gr a b -> [Node]
nodes = map fst . labNodes


-- | Find the context for the given 'Node'.  Causes an error if the 'Node' is
-- not present in the 'Graph'.
context :: (Graph gr) => gr a b -> Node -> Context a b
context g v = fromMaybe (error ("Match Exception, Node: "++show v))
                        (fst (match v g))

