{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Test.CmmPaths
  ( eventPaths
  , PathTrackable(..)
  , cmmExits
  )
where

import Prelude hiding (succ)

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch

import GHC.Test.ControlMonad

import GHC.Utils.Panic

--type Path' stmt exp = [Event stmt exp]
type APath event = [event]

class NonLocal node => PathTrackable node event where
  blockBodyEvent  :: Label -> Block node C C -> event
  blockExitEvents :: Label -> Block node C C -> [(Maybe event, Label)]

eventPaths :: forall node event .
               PathTrackable node event => GenCmmGraph node -> [[event]]
eventPaths g = map reverse $ pathsPrefixed (g_entry g) [] setEmpty
  where pathsPrefixed :: Label -> APath event -> LabelSet -> [APath event]
            -- ^ returns a list of all _short_ paths that begin with (block : prefix),
            -- where a short path is one that contains at most one repeated label,
            -- which must be the last one on the path (and so at the head of the list).
            -- Precondition: `visited == setFromList prefix`.
        pathsPrefixed lbl prefix visited = prefix' : extensions
          where prefix' = action lbl : prefix
                visited' = setInsert lbl visited
                extensions = if setMember lbl visited then [prefix']
                             else concatMap extend (blockExitEvents lbl $ blockLabeled lbl)
                extend (Nothing, lbl) = pathsPrefixed lbl prefix' visited'
                extend (Just event, lbl) = pathsPrefixed lbl (event : prefix') visited'


        action lbl = blockBodyEvent lbl (blockLabeled lbl)
        blockLabeled lbl = mapFindWithDefault (panic "missing block") lbl blockmap


        CmmGraph { g_graph = GMany NothingO blockmap NothingO } = g




{-
_oldEventpaths :: CmmGraph ->  [[Event Label Label]]
_oldEventpaths g = map reverse $ pathsPrefixed (g_entry g) [] setEmpty
  where pathsPrefixed :: Label -> Path' -> LabelSet -> [Path']
            -- ^ returns a list of all _short_ paths that begin with (block : prefix),
            -- where a short path is one that contains at most one repeated label,
            -- which must be the last one on the path (and so at the head of the list).
            -- Precondition: `visited == setFromList prefix`.
        pathsPrefixed lbl prefix visited = prefix' : extensions
          where prefix' = Action lbl : prefix
                visited' = setInsert lbl visited
                extensions = if setMember lbl visited then [prefix']
                             else concatMap extend (cmmExits $ blockLabeled lbl)
                extend (Nothing, lbl) = pathsPrefixed lbl prefix' visited'
                extend (Just event, lbl) = pathsPrefixed lbl (event : prefix') visited'


        blockLabeled lbl = mapFindWithDefault (panic "missing block") lbl blockmap


        CmmGraph { g_graph = GMany NothingO blockmap NothingO } = g
-}

type Labeled a = (Label, a)

instance PathTrackable CmmNode (Event (Labeled (Block CmmNode O O)) (Labeled CmmExpr)) where
  blockBodyEvent lbl b = Action (lbl, middle (blockSplit b))
    where middle (_, stmt, _) = stmt
  blockExitEvents _ = cmmExits

instance PathTrackable CmmNode (Event Label Label) where
  blockBodyEvent lbl _ = Action lbl
  blockExitEvents _lbl = map xlabel . cmmExits
     where xlabel (event, lbl) = (fmap (eventMap id fst) event, lbl)


cmmExits :: CmmBlock -> [(Maybe (Event stmt (Labeled CmmExpr)), Label)]
cmmExits b =
    case lastNode b of
      CmmBranch l -> [(Nothing, l)]
      CmmCondBranch e t f _ -> [(Just $ Predicate (thisExp e) True, t),
                                (Just $ Predicate (thisExp e) False, f)]
      CmmSwitch e targets ->
          let (lo, hi) = switchTargetsRange targets
              dests = switchTargetsCases targets
              other = switchTargetsDefault targets
              caseExit (j, lbl) = (Just $ Switch (thisExp e) (lo, hi + 1) j, lbl)
              defaultExits = case other of
                               Nothing -> []
                               Just lbl -> [(Just $ Switch (thisExp e) (lo, hi + 1) defarg, lbl)]
              defarg = try lo
                  where try i | i == hi = i
                              | i `elem` caseArgs = try (i + 1)
                              | otherwise = i
                        caseArgs = map fst dests
              labelOf i = case [lbl | (j, lbl) <- dests, j == i]
                          of [lbl] -> lbl
                             [] -> case other of
                                     Just lbl -> lbl
                                     Nothing -> panic "GHC.Tests.CmmPaths.exit: no default"
                             (_ : _ : _) -> panic "GHC.Tests.CmmPaths.exit: too many matches"
          in  if hi - lo < 10 then
                [(Just $ Switch (thisExp e) (lo, hi + 1) i, labelOf i) | i <- [lo..hi]]
              else
                  -- as some switch statements go from minBound :: Int to maxBound :: Int
                defaultExits ++ map caseExit dests

      CmmCall { cml_cont = Just l } -> [(Nothing, l)]
      CmmCall { cml_cont = Nothing } -> []
      CmmForeignCall { succ = l } -> [(Nothing, l)]
  where thisExp e = (entryLabel b, e)
