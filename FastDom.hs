{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module FastDom
  ( DominatorSet(..)
  , intersectDomSet
  , dominatorMap
  , dominatorMap'
  , domlattice

  , rpmap
  )
where 

import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm 

-- | An efficient data structure for representing dominator sets.
-- For details, see Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. 
-- A simple, fast dominance algorithm. 2006. 

data DominatorSet = NumberedNode { ds_revpostnum :: RPNum -- ^ reverse postorder number
                                 , ds_label  :: Label
                                 , ds_parent :: DominatorSet
                                    -- invariant: parent is never AllNodes
                                 } 
                  | EntryNode
                  | AllNodes -- equivalent of paper's Undefined

-- in reverse postorder, nodes closer to the entry have smaller numbers


type RPNum = Int  -- should this be a newtype?


-- N.B. The original paper uses a mutable data structure which is updated
-- imperatively.  NR thinks it likely that this aspect of the algorithm
-- is not essential to the good performance: what matters is that the
-- number of iterations be small and that intersections be computed quickly.
-- But there are no measurements.

intersectDomSet :: OldFact DominatorSet -> NewFact DominatorSet -> JoinedFact DominatorSet
intersectDomSet = intersectDomSet' NotChanged

intersectDomSet' :: (DominatorSet -> JoinedFact DominatorSet)
                 -> OldFact DominatorSet
                 -> NewFact DominatorSet
                 -> JoinedFact DominatorSet
intersectDomSet' nc (OldFact EntryNode) (NewFact _)         = nc EntryNode
intersectDomSet' _  (OldFact _)         (NewFact EntryNode) = Changed EntryNode
intersectDomSet' nc (OldFact a)         (NewFact AllNodes)  = nc a
intersectDomSet' _  (OldFact AllNodes)  (NewFact a)         = Changed a
intersectDomSet' nc ofct@(OldFact (NumberedNode old ol op))
                    nfct@(NewFact (NumberedNode new nl np))
  | old < new = intersectDomSet' nc ofct (NewFact np)
  | old > new = intersectDomSet' Changed (OldFact op) nfct
  | otherwise = nc (NumberedNode old ol op)


-- The rest of the code uses Cmm.Dataflow (Hoopl) to calculate
-- the dominators of each node.  Because it is not so easy to attach
-- a postorder number to each node, the code is a little awkward.

domlattice :: DataflowLattice DominatorSet
domlattice = DataflowLattice AllNodes intersectDomSet

dominatorMap :: forall node .
                (NonLocal node)
             => GenCmmGraph node
             -> LabelMap DominatorSet

dominatorMap' :: forall node .
                (NonLocal node)
             => GenCmmGraph node
             -> (LabelMap DominatorSet, LabelMap RPNum)
                 -- ^ includes reverse postorder numbering

dominatorMap = fst . dominatorMap'

dominatorMap' g =
  (analyzeCmmFwd domlattice transfer g startFacts, rpnums)
      where startFacts = mkFactBase domlattice [(g_entry g, EntryNode)]
            transfer block facts =
                asBase [(successor, NumberedNode (nodenum block) (entryLabel block) incoming)
                            | successor <- successors block]
                    where asBase = mkFactBase domlattice
                          incoming = getFact domlattice (entryLabel block) facts
            rpnums :: LabelMap Int
            -- ^ There's no easy way to put a reverse postorder number on each node,
            -- so those numbers are recorded here.
            rpnums = rpmap g
            nodenum :: Block node C C -> Int
            -- ^ reverse postorder number of each node
            nodenum block = mapFindWithDefault (-1) (entryLabel block) rpnums

rpmap :: forall node . (NonLocal node) => GenCmmGraph node -> LabelMap Int
rpmap g = mapFromList $ zip (map entryLabel rpblocks) [1..]
  where rpblocks = revPostorderFrom (graphMap g) (g_entry g)

graphMap :: GenCmmGraph n -> LabelMap (Block n C C)
graphMap (CmmGraph { g_graph = GMany NothingO blockmap NothingO }) = blockmap


instance Semigroup DominatorSet where
    d <> d' = getJoined (intersectDomSet (OldFact d) (NewFact d'))
      where getJoined (Changed a) = a
            getJoined (NotChanged a) = a

instance Monoid DominatorSet where
    mempty = AllNodes
