{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Cmm.Dataflow.Dominators
  ( DominatorSet(..)
  , GraphWithDominators(..)
  , RPNum
  , unreachableRPNum
  , intersectDomSet
  , graphWithDominators
  , domlattice

  , graphMap
  , gwdRPNumber
  , gwdDominatorsOf
  , dominatorsMember
  )
where 

import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm 

import GHC.Utils.Outputable(Outputable(..), text, int, hcat)
import GHC.Utils.Panic

-- | =Dominator sets

-- | An efficient data structure for representing dominator sets.
-- For details, see Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. 
-- "A simple, fast dominance algorithm." 2006. 
--
-- Also relevant: Loukas Georgiadis, Renato F. Werneck, Robert
-- E. Tarjan, Spyridon Triantafyllis, and David I. August (January 2006).  "Finding
-- Dominators in Practice."  Journal of Graph Algorithms and Applications 10(1):69-94.

data DominatorSet = NumberedNode { ds_revpostnum :: RPNum
                                 , ds_label  :: Label
                                 , ds_parent :: DominatorSet
                                    -- invariant: parent is never AllNodes
                                 } 
                  | EntryNode
                  | AllNodes -- equivalent of paper's Undefined

-- | Reverse postorder number of a node in a CFG
newtype RPNum = RPNum Int
  deriving (Eq, Ord)
-- in reverse postorder, nodes closer to the entry have smaller numbers


dominatorsMember :: Label -> DominatorSet -> Bool
-- ^ Tells if the given label is in the given
-- dominator set.  Which is to say, does the bloc
-- with with given label _properly_ and _non-vacuously_
-- dominate the node whose dominator set this is?
dominatorsMember lbl (NumberedNode _ l p) = l == lbl || dominatorsMember lbl p
dominatorsMember _   AllNodes  = False -- ^ see Note [Dominator Membership]
dominatorsMember _   EntryNode = False



{- [Note Dominator Memership]

Technically AllNodes is the universal set, of which every 
label should be a member.  But if a block B has dominator set 
is AllNodes, the dominator relation is vacuous: block B is
dominated by block A when all paths from the entry to B
include A.  But _there are no such paths_.  In such a situation
it is more useful to pretend that B has no dominators.

Also note that technically every block B dominates itself,
but a `DominatorSet` contains only the *proper* dominators.

-}



-- | `Monoid` instance; the operation is intersection.
instance Semigroup DominatorSet where
    d <> d' = getJoined (intersectDomSet (OldFact d) (NewFact d'))
      where getJoined (Changed a) = a
            getJoined (NotChanged a) = a

instance Monoid DominatorSet where
    mempty = AllNodes





-- N.B. The original paper uses a mutable data structure which is
-- updated imperatively.  This aspect of the algorithm is unlikely to
-- be essential to the good performance: what matters is that the
-- number of iterations be small and that intersections be computed
-- quickly.  But there are no measurements.

-- | Efficient intersection of dominator sets

intersectDomSet :: OldFact DominatorSet
                -> NewFact DominatorSet
                -> JoinedFact DominatorSet
intersectDomSet = intersectDomSet' NotChanged
 where
  intersectDomSet' :: (DominatorSet -> JoinedFact DominatorSet)
                   -> OldFact DominatorSet
                   -> NewFact DominatorSet
                   -> JoinedFact DominatorSet
  intersectDomSet' nc (OldFact EntryNode) (NewFact _)         = nc EntryNode
  intersectDomSet' _  (OldFact _)         (NewFact EntryNode) = Changed EntryNode
  intersectDomSet' nc (OldFact a)         (NewFact AllNodes)  = nc a
  intersectDomSet' _  (OldFact AllNodes)  (NewFact a)         = Changed a
  intersectDomSet' nc ofct@(OldFact (NumberedNode old ol op))
                      nfct@(NewFact (NumberedNode new _  np))
    | old < new = intersectDomSet' nc ofct (NewFact np)
    | old > new = intersectDomSet' Changed (OldFact op) nfct
    | otherwise = nc (NumberedNode old ol op)


-- | =Dominator computation via dataflow analysis

-- The code below uses Cmm.Dataflow (Hoopl) to calculate
-- the dominators of each node.  

domlattice :: DataflowLattice DominatorSet
domlattice = DataflowLattice AllNodes intersectDomSet

data GraphWithDominators node =
    GraphWithDominators { gwd_graph :: GenCmmGraph node
                        , gwd_dominators :: LabelMap DominatorSet
                        , gwd_rpnumbering :: LabelMap RPNum
                        }
  -- ^ Dominators and RP numberings include only *reachable* blocks.


graphWithDominators :: forall node .
                       (NonLocal node)
                       => GenCmmGraph node
                       -> GraphWithDominators node

-- XXX question for reviewer: should the graph returned be the original graph 
-- or a new graph containing only the reachable nodes?

graphWithDominators g = GraphWithDominators g dmap rpmap
      where dmap = analyzeCmmFwd domlattice transfer g startFacts

            startFacts = mkFactBase domlattice [(g_entry g, EntryNode)]
            transfer block facts =
                asBase [(successor, outgoing) | successor <- successors block]
             where asBase = mkFactBase domlattice
                   incoming = getFact domlattice (entryLabel block) facts
                   outgoing = NumberedNode (nodenum block) (entryLabel block) incoming

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [1..]
              where kvpair block i = (entryLabel block, RPNum i)
                    rpblocks = revPostorderFrom (graphMap g) (g_entry g)

            nodenum :: Block node C C -> RPNum
            -- ^ reverse postorder number of each node
            nodenum block = mapFindWithDefault unreachableRPNum (entryLabel block) rpmap


-- | =Utility functions

graphMap :: GenCmmGraph n -> LabelMap (Block n C C)
graphMap (CmmGraph { g_graph = GMany NothingO blockmap NothingO }) = blockmap

gwdRPNumber :: GraphWithDominators node -> Label -> RPNum
gwdRPNumber g l = mapFindWithDefault unreachableRPNum l (gwd_rpnumbering g)

gwdDominatorsOf :: GraphWithDominators node -> Label -> DominatorSet
gwdDominatorsOf g lbl =
    mapFindWithDefault (panic "label not reachable in graph") lbl (gwd_dominators g)


-- | =Further support for `RPNum`


unreachableRPNum :: RPNum
unreachableRPNum = RPNum (-1)



instance Show RPNum where
  show (RPNum i) = "RP" ++ show i

instance Outputable RPNum where
  ppr (RPNum i) = hcat [text "RP", int i]
   -- using `(<>)` would conflict with Semigroup
