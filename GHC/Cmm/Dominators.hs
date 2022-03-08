{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Cmm.Dominators
  ( DominatorSet(..)
  , GraphWithDominators(..)
  , RPNum
  , graphWithDominators

  , graphMap
  , gwdRPNumber
  , gwdDominatorsOf
  , dominatorsMember

  , intersectDominators
  )
where

import Data.Array.IArray
import Data.Foldable()

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import qualified GHC.CmmToAsm.CFG.Dominators as LT

import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm

import GHC.Utils.Outputable(Outputable(..), text, int, hcat, (<+>))
import GHC.Utils.Panic


-- | =Dominator sets
--
-- Node X dominates node Y if and only if every path from the entry to
-- Y includes X.  Node Y technically dominates itself, but it is
-- never included in the *representation* of its dominator set.
--
-- A dominator set is represented as a linked list in which each node
-- points to its *immediate* dominator, which is its parent in the
-- dominator tree.

data DominatorSet = ImmediateDominator { ds_label  :: Label
                                       , ds_parent :: DominatorSet
                                       }
                  | EntryNode
  deriving (Eq)

instance Outputable DominatorSet where
  ppr EntryNode = text "entry"
  ppr (ImmediateDominator l parent) = ppr l <+> text "->" <+> ppr parent



-- | Reverse postorder number of a node in a CFG
newtype RPNum = RPNum Int
  deriving (Eq, Ord)
-- in reverse postorder, nodes closer to the entry have smaller numbers

instance Show RPNum where
  show (RPNum i) = "RP" ++ show i

instance Outputable RPNum where
  ppr (RPNum i) = hcat [text "RP", int i]
   -- using `(<>)` would conflict with Semigroup



dominatorsMember :: Label -> DominatorSet -> Bool
-- ^ Tells if the given label is in the given
-- dominator set.  Which is to say, does the bloc
-- with with given label _properly_ and _non-vacuously_
-- dominate the node whose dominator set this is?
dominatorsMember lbl (ImmediateDominator l p) = l == lbl || dominatorsMember lbl p
dominatorsMember _   EntryNode = False


-- | Inefficient, and should be used only for things like visualizations
intersectDominators :: DominatorSet -> DominatorSet -> DominatorSet
intersectDominators ds ds' = commonPrefix (revDoms ds []) (revDoms ds' []) EntryNode
  where revDoms EntryNode prev = prev
        revDoms (ImmediateDominator lbl doms) prev = revDoms doms (lbl:prev)
        commonPrefix (a:as) (b:bs) doms
            | a == b = commonPrefix as bs (ImmediateDominator a doms)
        commonPrefix _ _ doms = doms


-- | The result of dominator analysis.  Also includes a reverse
-- postorder numbering, which is needed for dominator analysis.
--
-- Invariant: Dominators, graph, and RP numberings include only *reachable* blocks.
data GraphWithDominators node =
    GraphWithDominators { gwd_graph :: GenCmmGraph node
                        , gwd_dominators :: LabelMap DominatorSet
                        , gwd_rpnumbering :: LabelMap RPNum
                        }


-- use Lengauer-Tarjan from x86 back end
graphWithDominators :: forall node .
       (NonLocal node)
       => GenCmmGraph node
       -> GraphWithDominators node

graphWithDominators g = GraphWithDominators (reachable rpblocks g) dmap rpmap
      where rpblocks = revPostorderFrom (graphMap g) (g_entry g)
            rplabels' = map entryLabel rpblocks
            rplabels :: Array Int Label
            rplabels = listArray bounds rplabels'

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [0..]
              where kvpair block i = (entryLabel block, RPNum i)

            labelIndex :: Label -> Int
            labelIndex = flip (mapFindWithDefault undefined) imap
              where imap :: LabelMap Int
                    imap = mapFromList $ zip rplabels' [0..]
            blockIndex = labelIndex . entryLabel

            bounds = (0, length rpblocks - 1)

            ltGraph :: [Block node C C] -> LT.Graph
            ltGraph [] = IM.empty
            ltGraph (block:blocks) =
                IM.insert
                      (blockIndex block)
                      (IS.fromList $ map labelIndex $ successors block)
                      (ltGraph blocks)

            idom_array :: Array Int LT.Node
            idom_array = array bounds $ LT.idom (0, ltGraph rpblocks)

            domSet 0 = EntryNode
            domSet i = ImmediateDominator (rplabels ! d) (doms ! d)
                where d = idom_array ! i
            doms = tabulate bounds domSet

            dmap = mapFromList $ zipWith (\lbl i -> (lbl, domSet i)) rplabels' [0..]

reachable :: NonLocal node => [Block node C C] -> GenCmmGraph node -> GenCmmGraph node
reachable blocks g = g { g_graph = GMany NothingO blockmap NothingO }
  where blockmap = mapFromList [(entryLabel b, b) | b <- blocks]


-- | =Utility functions

graphMap :: GenCmmGraph n -> LabelMap (Block n C C)
graphMap (CmmGraph { g_graph = GMany NothingO blockmap NothingO }) = blockmap

gwdRPNumber :: GraphWithDominators node -> Label -> RPNum
gwdRPNumber g l = mapFindWithDefault unreachable l (gwd_rpnumbering g)

gwdDominatorsOf :: GraphWithDominators node -> Label -> DominatorSet
gwdDominatorsOf g lbl = mapFindWithDefault unreachable lbl (gwd_dominators g)

-- | Turn a function into an array.  Inspired by SML's `Array.tabulate`
tabulate :: (Ix i) => (i, i) -> (i -> e) -> Array i e
tabulate b f = listArray b $ map f $ range b

unreachable :: a
unreachable = panic "unreachable node in GraphWithDominators"
