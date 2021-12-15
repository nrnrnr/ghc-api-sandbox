{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow.Run
  ( evalWasm
  )
where

import Data.Maybe

import GHC.Wasm.ControlFlow

import GHC.Test.ControlMonad

-- data Atomic s e = Stmt s | Predicate e | Enum e

--class Monad m => WasmMonad m where
--  type WMExpr m
--  type WMStmt m
--
--  evalPredicate :: WMExpr m -> m Bool
--  evalEnum      :: Int -> WMExpr m -> m Int
--  takeAction    :: WMStmt m -> m ()
--
--  trap :: m ()
--  exitCrash :: Int -> m ()

data Frame s = EndLoop s | EndBlock | EndIf | Run s

evalWasm :: ControlTestMonad m => WasmStmt s e -> m ()
run  :: forall s e m . ControlTestMonad m => Stack s e -> m ()

type Stack s e = [Frame (WasmStmt s e)]

evalWasm s = run [Run s]


run [] = return ()
run (EndLoop s : stack) = run (Run s : EndLoop s : stack)
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Run s : stack) = step s
  where step :: WasmStmt s e -> m ()
        step WasmNop = run stack
        step (WasmUnreachable) = fail "unreachable"
        step (WasmBlock s) = run (Run (unL s) : EndBlock : stack)
        step (WasmLoop s) = run (Run (unL s) : EndLoop (unL s) : stack)
        step (WasmBr (BranchTyped bty k')) = exit (unL k') bty stack

        step (WasmIf e t f) = do
          b <- evalPredicate $ fromJust $ labelOf e
          run (Run (if b then t else f) : EndIf : stack)

        step (WasmBrIf e (BranchTyped bty k')) = do
          b <- evalPredicate $ fromJust $ labelOf e
          if b then exit (unL k') bty stack else run stack

        step (WasmBrTable e targets default') = do
          n <- fromInteger <$> evalEnum (fromJust $ labelOf e) (0, toInteger $ length targets)
          if n >= 0 && n < length targets then exit (unL (targets !! n)) ExitBranch stack
          else exit (unL default') ExitBranch stack
            

        step (WasmReturn) = return ()

        step (WasmSlc s) = (takeAction $ fromJust $ labelOf s) >> run stack
        step (WasmSeq s s') = run (Run s : Run s' : stack)

        step (WasmLabel _) = run stack

        exit 0 ContinueBranch (EndLoop s : stack) = run (EndLoop s : stack)
        exit 0 ExitBranch     (EndLoop _ : _) = fail "exit to loop header"
        exit 0 ExitBranch     (EndBlock : stack) = run stack
        exit 0 ContinueBranch (EndBlock : _) = fail "continue to block end"
        exit 0 ExitBranch     (EndIf : stack) = run stack
        exit 0 ContinueBranch (EndIf : _) = fail "continue to endif"
        exit k ty (Run _ : stack) = exit k ty stack
        exit k ty (_ : stack) = exit (pred k) ty stack
        exit _ _ [] = fail "exit too large"

        unL = withoutLabel
