{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Attic.PaperRun
  ( evalWasm
  )
where

import GHC.Cmm.Dataflow.Label
import GHC.Wasm.Paper

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

evalWasm :: ControlTestMonad Label m => WasmControl Label -> m ()
run  :: forall m . ControlTestMonad Label m => Stack Label -> m ()

type Stack s = [Frame (WasmControl s)]

evalWasm s = run [Run s]


run [] = return ()
run (EndLoop s : stack) = run (Run s : EndLoop s : stack)
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Run s : stack) = step s
  where step :: WasmControl Label -> m ()
--        step WasmNop = run stack
--        step (WasmUnreachable) = fail "unreachable"
        step (WasmBlock s) = run (Run s : EndBlock : stack)
        step (WasmLoop s) = run (Run s : EndLoop s : stack)
        step (WasmBr k') = exit k' stack

        step (WasmIf e t f) = do
          b <- evalPredicate e
          run (Run (if b then t else f) : EndIf : stack)

--        step (WasmBrIf e k') = do
--          b <- evalPredicate e
--          if b then exit k' stack else run stack

        step (WasmBrTable e range targets default') = do
          n <- fromInteger <$>
               evalEnum e (bti_lo range, bti_lo range + bti_count range)
          if n >= 0 && n < length targets then exit (targets !! n) stack
          else exit default' stack


        step (WasmReturn) = return ()

        step (WasmSlc s) = takeAction s >> run stack
        step (WasmSeq s s') = run (Run s : Run s' : stack)

        exit 0 (EndLoop s : stack) = run (EndLoop s : stack)
        exit 0 (EndBlock : stack) = run stack
        exit 0 (EndIf : stack) = run stack
        exit k (Run _ : stack) = exit k stack
        exit k (_ : stack) = exit (pred k) stack
        exit _ [] = fail "exit too large"
