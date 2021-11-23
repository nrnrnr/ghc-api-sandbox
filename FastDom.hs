{-# OPTIONS_GHC -Wno-orphans #-}

module FastDom
where 

import GHC.Cmm.Dataflow

-- | An efficient data structure for representing dominator sets.
-- For details, see Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. 
-- A simple, fast dominance algorithm. 2006. 

data DominatorSet = NumberedNode { ds_postnum :: Int -- ^ postorder number
                                 , ds_parent :: DominatorSet
                                 } 
                  | EntryNode

-- in postorder, nodes closer to the entry have larger numbers

intersectMaybeDomSet :: OldFact (Maybe DominatorSet)
                     -> NewFact (Maybe DominatorSet)
                     -> JoinedFact (Maybe DominatorSet)
-- Here "Nothing" stands for "all nodes" (or "Undefined")
intersectMaybeDomSet (OldFact a)  (NewFact Nothing)  = NotChanged a
intersectMaybeDomSet (OldFact Nothing) (NewFact a)         = Changed a
intersectMaybeDomSet (OldFact (Just old)) (NewFact (Just new)) =
    Just <$> intersectDomSet (OldFact old) (NewFact new)


intersectDomSet :: OldFact DominatorSet -> NewFact DominatorSet -> JoinedFact DominatorSet
intersectDomSet = intersectDomSet' NotChanged

intersectDomSet' :: (DominatorSet -> JoinedFact DominatorSet)
                 -> OldFact DominatorSet
                 -> NewFact DominatorSet
                 -> JoinedFact DominatorSet
intersectDomSet' nc (OldFact EntryNode)    (NewFact _)         = nc EntryNode
intersectDomSet' _  (OldFact _)        (NewFact EntryNode)     = Changed EntryNode
intersectDomSet' nc ofct@(OldFact (NumberedNode old op)) nfct@(NewFact (NumberedNode new np))
  | old > new = intersectDomSet' nc ofct (NewFact np)
  | old < new = intersectDomSet' Changed (OldFact op) nfct
  | otherwise = nc (NumberedNode old op)


instance Functor JoinedFact where
    fmap f (Changed a) = Changed (f a)
    fmap f (NotChanged a) = NotChanged (f a)
