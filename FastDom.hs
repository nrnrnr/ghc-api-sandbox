module FastDom
where 

import GHC.Cmm.Dataflow

-- | An efficient data structure for representing dominator sets.
-- For details, see Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. 
-- A simple, fast dominance algorithm. 2006. 

data DominatorSet = NumberedNode { ds_revpostnum :: Int -- ^ reverse postorder number
                                 , ds_parent :: DominatorSet
                                    -- invariant: parent is never AllNodes
                                 } 
                  | EntryNode
                  | AllNodes -- equivalent of paper's Undefined

-- in reverse postorder, nodes closer to the entry have smaller numbers

intersectDomSet :: OldFact DominatorSet -> NewFact DominatorSet -> JoinedFact DominatorSet
intersectDomSet = intersectDomSet' NotChanged

intersectDomSet' :: (DominatorSet -> JoinedFact DominatorSet)
                 -> OldFact DominatorSet
                 -> NewFact DominatorSet
                 -> JoinedFact DominatorSet
intersectDomSet' nc (OldFact EntryNode)    (NewFact _)         = nc EntryNode
intersectDomSet' _  (OldFact _)        (NewFact EntryNode)     = Changed EntryNode
intersectDomSet' nc (OldFact a)        (NewFact AllNodes)     = nc a
intersectDomSet' _  (OldFact AllNodes)    (NewFact a)         = Changed a
intersectDomSet' nc ofct@(OldFact (NumberedNode old op)) nfct@(NewFact (NumberedNode new np))
  | old < new = intersectDomSet' nc ofct (NewFact np)
  | old > new = intersectDomSet' Changed (OldFact op) nfct
  | otherwise = nc (NumberedNode old op)


