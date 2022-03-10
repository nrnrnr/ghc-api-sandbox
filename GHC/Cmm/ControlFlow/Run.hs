{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Cmm.ControlFlow.Run
  ( evalGraph
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


evalGraph :: forall m . ControlTestMonad Label Label m => CmmGraph -> m ()
evalGraph g = run (g_entry g)
  where GMany NothingO blockmap NothingO = g_graph g
        run :: Label -> m ()
        run label = do
          takeAction @Label @Label label
          case lastNode (blockOf label) of
            CmmBranch l -> run l
            CmmCondBranch _ t f _ -> do
                      b <- evalPredicate @Label @Label label
                      run (if b then t else f)
            CmmSwitch _ targets -> do
                      i <- evalEnum @Label @Label label $ extendRight $ switchTargetsRange targets
                      run $ labelIn i targets

            CmmCall { cml_cont = Just l } -> run l
            CmmCall { cml_cont = Nothing } -> return ()
            CmmForeignCall { succ = l } -> run l

        blockOf lbl =
             mapFindWithDefault (panic "GHC.Cmm.ControlFlow.Run.eval") lbl blockmap

extendRight :: Integral n => (n, n) -> (n, n)
extendRight (lo, hi) = (lo, hi + 1)

labelIn :: Integer -> SwitchTargets -> Label
labelIn i targets =
    case [lbl | (j, lbl) <- switchTargetsCases targets, j == i]
      of [lbl] -> lbl
         [] -> case switchTargetsDefault targets of
                 Just lbl -> lbl
                 Nothing -> panic "GHC.Cmm.ControlFlow.Run.labelIn: no default"
         (_ : _ : _) -> panic "GHC.Cmm.ControlFlow.Run: too many matches"
