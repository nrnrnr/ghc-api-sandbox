{-# LANGUAGE TypeFamilies #-}

module GHC.Cmm.LoopInfo
  ( LoopInfo(..), Edge(..)
  , loopInfo
  )
where

import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

data Edge = Edge { edge_from :: Label, edge_to :: Label }

data LoopInfo = LoopInfo
  { liBackEdges :: [Edge] -- ^ List of back edges
  , liHeaders :: LabelSet 
  , liLoops :: [(Edge, LabelSet)] -- ^ Natural loop containing a given back edge;
                                  -- (backEdge, loopBody) where body includes header
                                  
  , liLevels :: LabelMap Int  -- number of natural loops in which need node appears,
                              -- default 0
  }


loopInfo :: NonLocal node => GraphWithDominators node -> LoopInfo
loopInfo gwd = LoopInfo backEdges headers loops levels
  where backEdges = [Edge lbl succ | (lbl, b) <- mapToList blockmap,
                                     succ <- successors b,
                                     rpnum succ <= rpnum lbl]

        l `dominates` l' = l == l' || dominatorsMember l (gwdDominatorsOf gwd l')
        headers = setFromList [succ | Edge lbl succ <- backEdges, succ `dominates` lbl]

        predmap :: LabelMap [Label]
        predmap = mapFoldlWithKey addBlock mapEmpty blockmap
            where addBlock pm l b =
                      foldl (\pm succ -> mapAlter cons_l succ pm) pm (successors b)
                    where cons_l Nothing = Just [l]
                          cons_l (Just ls) = Just (l : ls)

        preds :: Label -> [Label]
        preds l = mapFindWithDefault [] l predmap

        naturalLoopOf :: Edge -> LabelSet 
        naturalLoopOf (Edge from to) = visitPreds from (setSingleton to)
          where visitPreds lbl visited
                    | setMember lbl visited = visited
                    | otherwise = visitNodes (preds lbl) (setInsert lbl visited)
                visitNodes [] visited = visited
                visitNodes (l:ls) visited = visitNodes ls (visitPreds l visited)
        loops = map (\e -> (e, naturalLoopOf e)) backEdges                                          
        levels = foldl addLoop mapEmpty loops
          where addLoop labelmap (_, labels) = setFoldl bump labelmap labels
                bump labelmap label = mapInsert label (succ $ count label labelmap) labelmap
                count = mapFindWithDefault 0

        blockmap = graphMap (gwd_graph gwd)
        rpnum = gwdRPNumber gwd

-- A node is a loop header if it is a target 
-- of a back edge from a node it dominates.
-- (This definition works even for irreducible
-- graphs, for which it finds fewer headers
-- than simply "target of a back edge.")
