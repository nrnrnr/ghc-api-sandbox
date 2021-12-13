module GHC.Cmm.LoopInfo
  ( LoopInfo(..), Edge(..)
  , loopInfo
  )
where

import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

import GHC.Utils.Panic

data Edge = Edge { edge_from :: Label, edge_to :: Label }

data LoopInfo = LoopInfo
  { liBackEdges :: [Edge] -- ^ List of back edges
  , liHeaders :: LabelSet 
  , liLevels :: LabelMap Int
        -- ^ BlockId -> LoopLevel mapping, where loop level is the 
        -- number of loop headers that (properly?) dominate the block
--  , liLoops :: [(Edge, LabelSet)] -- ^ (backEdge, loopBody), body includes header
  }


loopInfo :: NonLocal node => GraphWithDominators node -> LoopInfo
loopInfo gwd = LoopInfo backEdges headers levels
  where backEdges = [Edge lbl succ | (lbl, b) <- mapToList blockmap,
                                     succ <- successors b,
                                     rpnum succ <= rpnum lbl]

        l `dominates` l' = l == l' || dominatorsMember l (gwdDominatorsOf gwd l')
        headers = setFromList [succ | Edge lbl succ <- backEdges, succ `dominates` lbl]

        levels = mapMap headerCount $ gwd_dominators gwd
        headerCount EntryNode = 0
        headerCount AllNodes = panic "AllNodes in dominator set"
        headerCount (NumberedNode _ lbl parent) =
            (if setMember lbl headers then 1 else 0) + headerCount parent

        blockmap = graphMap (gwd_graph gwd)
        rpnum = gwdRPNumber gwd




-- A node is a loop header if it is a target 
-- of a back edge from a node it dominates.
-- (This definition works even for irreducible
-- graphs, for which it finds fewer headers
-- than simply "target of a back edge.")
