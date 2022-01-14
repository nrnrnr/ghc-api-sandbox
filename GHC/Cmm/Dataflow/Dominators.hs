{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Cmm.Dataflow.Dominators
  ( DominatorSet(..)
  , GraphWithDominators(..)
  , RPNum
  , unreachableRPNum
  , intersectDomSet
  , intersectDominatorsSlow
  , graphWithDominators
  , graphWithDominators'
  , domlattice

  , graphMap
  , gwdRPNumber
  , gwdDominatorsOf
  , dominatorsMember
  )
where

import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)

import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm

import GHC.Utils.Outputable(Outputable(..), text, int, hcat, (<+>), parens, showSDocUnsafe)
import GHC.Utils.Panic
import Control.Monad.ST
--import Debug.Trace

trace :: String -> a -> a
trace _ a = a

-- | =Dominator sets

-- | An efficient data structure for representing dominator sets.
-- For details, see Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. 
-- "A simple, fast dominance algorithm." 2006. 
--
-- Also relevant: Loukas Georgiadis, Renato F. Werneck, Robert
-- E. Tarjan, Spyridon Triantafyllis, and David I. August (January 2006).  "Finding
-- Dominators in Practice."  Journal of Graph Algorithms and Applications 10(1):69-94.

data DominatorSet = ImmediateDominator { ds_label  :: Label
                                       , ds_parent :: DominatorSet
                                       }
                  | EntryNode
  deriving (Eq)

type DominatorSet' = LegacyDominatorSet
data LegacyDominatorSet = NumberedNode' { _ds_revpostnum' :: RPNum
                                        , ds_label'  :: Label
                                        , ds_parent' :: LegacyDominatorSet
                                    -- invariant: parent is never AllNodes
                                 }
                  | EntryNode'
                  | AllNodes' -- equivalent of paper's Undefined
  deriving (Eq)

unLegacy :: LegacyDominatorSet -> DominatorSet
unLegacy EntryNode' = EntryNode
unLegacy NumberedNode' { ds_label' = l, ds_parent' = p } = ImmediateDominator l (unLegacy p)
unLegacy AllNodes' = panic "GHC.Cmm.Dataflow.Dominators: undefined dominator"

-- | Reverse postorder number of a node in a CFG
newtype RPNum = RPNum Int
  deriving (Eq, Ord)
-- in reverse postorder, nodes closer to the entry have smaller numbers


dominatorsMember :: Label -> DominatorSet -> Bool
-- ^ Tells if the given label is in the given
-- dominator set.  Which is to say, does the bloc
-- with with given label _properly_ and _non-vacuously_
-- dominate the node whose dominator set this is?
dominatorsMember lbl (ImmediateDominator l p) = l == lbl || dominatorsMember lbl p
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
instance Semigroup LegacyDominatorSet where
    d <> d' = getJoined (intersectDomSet (OldFact d) (NewFact d'))
      where getJoined (Changed a) = a
            getJoined (NotChanged a) = a

instance Monoid LegacyDominatorSet where
    mempty = AllNodes'





-- N.B. The original paper uses a mutable data structure which is
-- updated imperatively.  Tragically, this aspect of the algorithm is
-- essential to the good performance; without the imperative update,
-- intersections have to be computed all the way up the chain to the root.
-- Bletch.

-- | Intersection of dominator sets

intersectDomSet :: OldFact DominatorSet'
                -> NewFact DominatorSet'
                -> JoinedFact DominatorSet'
intersectDomSet (OldFact old) (NewFact new) = let answer = intersectDomSet' NotChanged (OldFact old) (NewFact new)
                          in  (trace $ showSDocUnsafe (ppr old <+> text "`inter`" <+> ppr new <+> text "==" <+> ppr (strip answer)))  answer
 where
  intersectDomSet' :: (DominatorSet' -> JoinedFact DominatorSet')
                   -> OldFact DominatorSet'
                   -> NewFact DominatorSet'
                   -> JoinedFact DominatorSet'
  intersectDomSet' nc (OldFact EntryNode') (NewFact _)         = nc EntryNode'
  intersectDomSet' _  (OldFact _)         (NewFact EntryNode') = Changed EntryNode'
  intersectDomSet' nc (OldFact a)         (NewFact AllNodes')  = nc a
  intersectDomSet' _  (OldFact AllNodes')  (NewFact a)         = Changed a
  intersectDomSet' nc ofct@(OldFact (NumberedNode' old ol op))
                      nfct@(NewFact (NumberedNode' new _  np))
    | old < new = intersectDomSet' nc ofct (NewFact np)
    | old > new = intersectDomSet' Changed (OldFact op) nfct
    | otherwise = NumberedNode' old ol <$> intersectDomSet' nc (OldFact op) (NewFact np)

-- In the mutable version, the `otherwise` case just returns the old dominator
-- set.  If we could prove that this computation produced the correct _immediate_
-- dominator, then we could recover the full dominator tree from the immediate dominators.

instance Functor JoinedFact where
  fmap f (Changed a) = Changed (f a)
  fmap f (NotChanged a) = NotChanged (f a)

strip :: JoinedFact a -> a
strip (Changed a) = a
strip (NotChanged a) = a


-- | =Dominator computation via dataflow analysis

-- The code below uses Cmm.Dataflow (Hoopl) to calculate
-- the dominators of each node.  

domlattice :: DataflowLattice DominatorSet'
domlattice = DataflowLattice AllNodes' intersectDomSet

data GraphWithDominators node =
    GraphWithDominators { gwd_graph :: GenCmmGraph node
                        , gwd_dominators :: LabelMap DominatorSet
                        , gwd_rpnumbering :: LabelMap RPNum
                        }
  -- ^ Dominators and RP numberings include only *reachable* blocks.


graphWithDominators, graphWithDominators', graphWithDominators'', graphWithDominators'''
    :: forall node .
       (NonLocal node)
       => GenCmmGraph node
       -> GraphWithDominators node

-- XXX question for reviewer: should the graph returned be the original graph 
-- or a new graph containing only the reachable nodes?
-- ANSWER: new graph

-- plan A: use existing dataflow framework, requires slow intersection

graphWithDominators' g = GraphWithDominators g (fmap unLegacy dmap) rpmap
      where dmap = analyzeCmmFwd domlattice transfer g startFacts

            startFacts = mkFactBase domlattice [(g_entry g, EntryNode')]
            transfer block facts =
                asBase [(successor, outgoing) | successor <- successors block]
             where asBase = mkFactBase domlattice
                   incoming = getFact domlattice (entryLabel block) facts
                   outgoing = NumberedNode' (nodenum block) (entryLabel block) incoming

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [0..]
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


instance Outputable LegacyDominatorSet where
  ppr EntryNode' = text "entry"
  ppr (NumberedNode' n l parent) = hcat[ppr l, parens (ppr n)] <+> text "->" <+> ppr parent
  ppr AllNodes' = text "<all-nodes>"

instance Outputable DominatorSet where
  ppr EntryNode = text "entry"
  ppr (ImmediateDominator l parent) = ppr l <+> text "->" <+> ppr parent


----------------------------------------------------------------

-- Plan B: use an array

iterateFun :: (Eq a) => (a -> a) -> (a -> a)
iterateFun f a = if a' == a then a else iterateFun f a'
    where a' = trace "iterate array function" $ f a

tabulate :: (Ix i) => (i, i) -> (i -> e) -> Array i e
tabulate b f = listArray b $ map f $ range b


newtype IDom = IDom { unIDom :: Int }
  deriving (Eq, Ord)

instance Show IDom where
  show (IDom n) = "ID-"++show n




-- try them all and check consistency

graphWithDominators g = traceFaults g' (traceFaults g'' g''')
  where g' = graphWithDominators' g
        g'' = graphWithDominators'' g
        g''' = graphWithDominators''' g

        traceFaults :: GraphWithDominators node -> GraphWithDominators node -> GraphWithDominators node
        traceFaults g' g'' = check faults
         where
          d' = gwd_dominators g'
          d'' = gwd_dominators g''

          faults = [ (label, doms', doms'')
                   | (label, doms') <- mapToList d'
                   , let doms'' = mapFindWithDefault undefined label d''
                   , doms' /= doms''
                   ]

          check [] =
              if d' == d'' then trace "dominators match" g'
              else panic "dominators don't match"
          check ((l, ds', ds''):faults) =
              trace (showSDocUnsafe $ "label" <+> ppr l <+>
                     parens (hcat [ppr $ rpnum l, text ","] <+> ppr (rpnum' l)) <+>
                     ": old doms" <+> ppr ds' <+> "; new doms" <+> ppr ds'') $
                    check faults

          rpnum lbl = mapFindWithDefault undefined lbl (gwd_rpnumbering g')
          rpnum' lbl = mapFindWithDefault undefined lbl (gwd_rpnumbering g'')


-- plan B: fixed point of functional array -> array update

graphWithDominators'' g = GraphWithDominators g dmap rpmap
      where rpblocks = revPostorderFrom (graphMap g) (g_entry g)
            rplabels' = map entryLabel rpblocks
            rplabels :: Array Int Label
            rplabels = listArray bounds rplabels'
            labelIndex = flip (mapFindWithDefault undefined) imap
              where imap :: LabelMap Int
                    imap = mapFromList $ zip rplabels' [0..]
            blockIndex = labelIndex . entryLabel

            -- we don't store node 0.  Its immediate dominator is always the entry.
            bounds = (0, length rpblocks - 1)
            nonentryBounds = (1, length rpblocks - 1)

            poke a = trace "poking element 2" $ trace (show (a!2)) $ a

            -- each node originally contains a predecessor
            aPred i = head $ filter (<i) $ preds ! i
            idom_array = traceDoms $
                         trace ("Labels: " ++ show (fmap (showSDocUnsafe . ppr) rplabels)) $
                         iterateFun update $
                         checkClimbing nonentryBounds $
                         poke $
                         trace "first array" $
                         listArray nonentryBounds $ map (IDom . aPred) [1..]
            domSet 0 = EntryNode
            domSet i = ImmediateDominator (rplabels ! d) (doms ! d)
                where IDom d = idom_array ! i
            doms = tabulate bounds domSet


            traceDoms d = trace ("idoms == " ++ show d) d

            dmap = mapFromList $ zipWith (\lbl i -> (lbl, domSet i)) rplabels' [0..]

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [0..]
              where kvpair block i = (entryLabel block, RPNum i)



            preds :: Array Int [Int]
            preds = accumArray (flip (:)) [] nonentryBounds
                    [ (labelIndex to, blockIndex from)
                          | from <- rpblocks, to <- successors from ]

            update :: Array Int IDom -> Array Int IDom
            update a = traceUpd $ trace ("update " ++ show a) $ fmap intersectAll preds
                where intersectAll :: [Int] -> IDom
                      intersectAll is =
                          traceIdom $ trace (showInt is) $ foldl1 intersect (map IDom is)

                      intersect :: IDom -> IDom -> IDom
                      intersect (IDom i) (IDom j) =
                          case i `compare` j of
                            LT -> IDom i `intersect` (a ! j)
                            EQ -> IDom i
                            GT -> (a ! i) `intersect` IDom j

                      traceUpd a = trace ("updated: " ++ show a) a
            traceIdom id = trace ("intersection is " ++ show id) id
            showInt is = "intersecting " ++ show is

checkClimbing :: (Int, Int) -> Array Int IDom -> Array Int IDom
checkClimbing (lo, hi) a
    | lo > hi = a
    | unIDom (a ! lo) < lo = checkClimbing (lo+1, hi) a
    | otherwise = panic $ "a[" ++ show lo ++ "] == " ++ show (a!lo)


-- plan C: do it node by node with a mutable array
--
-- The advantage of this approach is that when the array is updated,
-- later nodes can profit from updates in the earlier nodes, instead
-- of having to wait for the next array-update cycle.  

newtype IDomArray s = IDA { unIDA :: (STUArray s Int Int) }
idaFromList :: (Int, Int) -> [IDom] -> ST s (IDomArray s)
idaRead  :: IDomArray s -> Int -> ST s IDom
idaWrite :: IDomArray s -> Int -> IDom -> ST s ()
runIda :: (forall s . ST s (IDomArray s)) -> UArray Int Int
--idaSub :: UArray Int Int -> Int -> IDom

idaFromList bounds elems = IDA <$> newListArray bounds (map unIDom elems)
idaRead (IDA a) i = IDom <$> readArray a i
idaWrite (IDA a) i (IDom d) = writeArray a i d
runIda a = runSTUArray (a >>= (return . unIDA))
--idaSub a i = IDom (a ! i)


data ThisChange = ThisChanged | ThisUnchanged

graphWithDominators''' g = GraphWithDominators g dmap rpmap
      where rpblocks = revPostorderFrom (graphMap g) (g_entry g)
            rplabels' = map entryLabel rpblocks
            rplabels :: Array Int Label
            rplabels = listArray bounds rplabels'
            labelIndex :: Label -> Int
            labelIndex = flip (mapFindWithDefault undefined) imap
              where imap :: LabelMap Int
                    imap = mapFromList $ zip rplabels' [0..]
            blockIndex = labelIndex . entryLabel

            -- we don't store node 0.  Its immediate dominator is always the entry.
            bounds = (0, length rpblocks - 1)
            nonentryBounds = (1, length rpblocks - 1)

            -- each node originally contains a predecessor
            aPred i = head $ filter (<i) $ preds ! i
            idom_array :: UArray Int Int
            idom_array = runIda go
               where go :: forall s . ST s (IDomArray s)
                     go = do a <- idaFromList nonentryBounds $ map (IDom . aPred) [1..]
                             update ThisChanged a
            domSet 0 = EntryNode
            domSet i = ImmediateDominator (rplabels ! d) (doms ! d)
                where d = idom_array ! i
            doms = tabulate bounds domSet


--            traceDoms d = trace ("idoms == " ++ show d) d

            dmap = mapFromList $ zipWith (\lbl i -> (lbl, domSet i)) rplabels' [0..]

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [0..]
              where kvpair block i = (entryLabel block, RPNum i)



            preds :: Array Int [Int]
            preds = accumArray (flip (:)) [] nonentryBounds
                    [ (labelIndex to, blockIndex from)
                          | from <- rpblocks, to <- successors from ]

            update :: forall s . ThisChange -> IDomArray s -> ST s (IDomArray s)
            update ThisUnchanged a = return a
            update ThisChanged a = -- trace "iterate node transfer" $
                                   transfer ThisUnchanged rpblocks 0
              where transfer :: ThisChange -> [Block node C C] -> Int -> ST s (IDomArray s)
                    transfer change [] _ = update change a
                    transfer change (block:blocks) i =
                        propagateTo change $ map labelIndex $ successors block
                      where propagateTo :: ThisChange -> [Int] -> ST s (IDomArray s)
                            propagateTo change (succ:succs) =
                                do old <- idaRead a succ
                                   new <- intersect old (IDom i)
                                   if old == new then
                                       propagateTo change  succs
                                   else
                                       idaWrite a succ new >>
                                       propagateTo ThisChanged succs
                            propagateTo change [] = transfer change blocks i
                    intersect :: IDom -> IDom -> ST s IDom
                    intersect (IDom i) (IDom j) =
                        case i `compare` j of
                          LT -> do { next <- idaRead a j; IDom i `intersect` next }
                          EQ -> return $ IDom i
                          GT -> do { next <- idaRead a i; next `intersect` IDom j }





{-
            startFacts = mkFactBase domlattice [(g_entry g, EntryNode)]
            transfer block facts =
                asBase [(successor, outgoing) | successor <- successors block]
             where asBase = mkFactBase domlattice
                   incoming = getFact domlattice (entryLabel block) facts
                   outgoing = NumberedNode (nodenum block) (entryLabel block) incoming


            nodenum :: Block node C C -> RPNum
            -- ^ reverse postorder number of each node
            nodenum block = mapFindWithDefault unreachableRPNum (entryLabel block) rpmap
-}


intersectDominatorsSlow :: DominatorSet -> DominatorSet -> DominatorSet
intersectDominatorsSlow ds ds' = commonPrefix (revDoms ds []) (revDoms ds' []) EntryNode
  where revDoms EntryNode prev = prev
        revDoms (ImmediateDominator lbl doms) prev = revDoms doms (lbl:prev)
        commonPrefix (a:as) (b:bs) doms
            | a == b = commonPrefix as bs (ImmediateDominator a doms)
        commonPrefix _ _ doms = doms

