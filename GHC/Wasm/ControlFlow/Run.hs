{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow.Run
  ( eval
  )
where

import GHC.Wasm.ControlFlow

-- data Atomic s e = Stmt s | Predicate e | Enum e

class Monad m => WasmMonad m where
  type WMExpr m
  type WMStmt m

  evalPredicate :: WMExpr m -> m Bool
  evalEnum      :: Int -> WMExpr m -> m Int
  takeAction    :: WMStmt m -> m ()

  trap :: m ()
  exitCrash :: Int -> m ()

data Frame s = EndLoop s | EndBlock | EndIf | Run s

eval :: WasmMonad m => WasmStmt (WMStmt m) (WMExpr m) -> m ()
run  :: forall m . WasmMonad m => Stack m -> m ()

type Stack m = [Frame (Code m)]
type Code m = WasmStmt (WMStmt m) (WMExpr m)

eval s = run [Run s]


run [] = return ()
run (EndLoop s : stack) = run (Run s : EndLoop s : stack)
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Run s : stack) = step s
  where step :: Code m -> m ()
        step WasmNop = run stack
        step (WasmUnreachable) = trap
        step (WasmBlock s) = run (Run (unL s) : EndBlock : stack)
        step (WasmLoop s) = run (Run (unL s) : EndLoop (unL s) : stack)
        step (WasmBr (BranchTyped _ k')) = exit (unL k') stack
        step (WasmIf e t f) = do
          b <- evalPredicate (unL e)
          run (Run (if b then t else f) : EndIf : stack)

        step (WasmBrIf e (BranchTyped _ k')) = do
          b <- evalPredicate e
          if b then exit (unL k') stack else run stack

        step (WasmBrTable e targets default') = do
          n <- evalEnum (length targets) e
          if n >= 0 && n < length targets then exit (unL (targets !! n)) stack
          else exit (unL default') stack
            

        step (WasmReturn) = return ()

        step (WasmSlc s) = takeAction s >> run stack
        step (WasmSeq s s') = run (Run s : Run s' : stack)

        step (WasmLabel _) = run stack

        exit 0 (EndLoop s : stack) = run (EndLoop s : stack)
        exit 0 (EndBlock : stack) = run stack
        exit 0 (EndIf : stack) = run stack
        exit k (Run _ : stack) = exit k stack
        exit k (_ : stack) = exit (pred k) stack
        exit k [] = exitCrash k

        unL = withoutLabel
