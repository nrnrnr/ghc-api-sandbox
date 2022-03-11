{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Wasm.ControlFlow.Run
  ( evalWasm
  )
where

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

evalWasm :: ControlTestMonad s e m => WasmControl s e -> m ()
run  :: forall s e m . ControlTestMonad s e m => Stack s e -> m ()

type Stack s e = [Frame (WasmControl s e)]

evalWasm s = run [Run s]


run [] = return ()
run (EndLoop s : stack) = run (Run s : EndLoop s : stack)
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Run s : stack) = step s
  where step :: WasmControl s e -> m ()
        step WasmNop = run stack
--        step (WasmComment _) = run stack
        step (WasmUnreachable) = fail "unreachable"
        step (WasmBlock s) = run (Run s : EndBlock : stack)
        step (WasmLoop s) = run (Run s : EndLoop s : stack)
        step (WasmBr k) = exit k stack

        step (WasmIf e t f) = do
          b <- evalPredicate @s @e e
          run (Run (if b then t else f) : EndIf : stack)

--        step (WasmBrIf e k) = do
--          b <- evalPredicate @s @e $ fromJust $ labelOf e
--          if b then exit k stack else run stack

        step (WasmBrTable e range targets default') = do
          n <- fromInteger <$>
               evalEnum @s @e e (bti_lo range, bti_lo range + bti_count range)
          if n >= 0 && n < length targets then exit (targets !! n) stack
          else exit default' stack


        step (WasmReturn) = return ()

        step (WasmSlc s) = takeAction @s @e s >> run stack
        step (WasmSeq s s') = run (Run s : Run s' : stack)

        exit 0 (EndLoop s : stack) = run (EndLoop s : stack)
        exit 0 (EndBlock : stack) = run stack
        exit 0 (EndIf : stack) = run stack
        exit k (Run _ : stack) = exit k stack
        exit k (_ : stack) = exit (pred k) stack
        exit _ [] = fail "exit too large"
