{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow
  ( WasmControl(..)
  , BrTableInterval(..), inclusiveInterval
  , wasmPeepholeOpt
  )
where

--import Data.Void

--import Debug.Trace

import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

-- | Models the control-flow portion of the WebAssembly instruction set



-- [Note block types]
--
-- WebAssembly blocks are normally labeled with a function type,
-- which specifies what values the block expects to find on the
-- WebAssembly evaluation stack and what values it promises to
-- leave there.  Those types do not appear in this representation.
-- The representation assumes that either the stack is left
-- unchanged by every block (Wasm type `[] -> []`) or that if
-- other types are needed, they will be computed by running
-- an inference algorithm over the code.


data WasmControl s e where
  -- ^ Type parameter `s` is the type of (unspecified) statements.
  -- It might be instantiated with an open Cmm block or with a sequence
  -- of Wasm instructions.
  -- Parameter `e` is the type of expressions.

  WasmBlock :: WasmControl s e -> WasmControl s e
  WasmLoop  :: WasmControl s e -> WasmControl s e
  WasmIf    :: e -> WasmControl s e -> WasmControl s e -> WasmControl s e

  WasmBr   :: Int -> WasmControl s e
  WasmBrTable :: e
              -> BrTableInterval -- for debugging only
              -> [Int]
              -> Int
              -> WasmControl s e
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturn :: WasmControl s e

  WasmSlc :: s -> WasmControl s e   -- straight-line code
  WasmSeq :: WasmControl s e -> WasmControl s e -> WasmControl s e

  WasmUnreachable :: WasmControl s e
  WasmNop :: WasmControl s e

data BrTableInterval
    = BrTableInterval { bti_lo :: Integer, bti_count :: Integer }

instance Outputable BrTableInterval where
  ppr range = brackets $ hcat[integer (bti_lo range), text "..", integer hi]
      where hi = bti_lo range + bti_count range - 1

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = BrTableInterval lo (hi - lo + 1)
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

instance Semigroup (WasmControl s e) where
  (<>) WasmNop a = a
  (<>) a WasmNop = a
  (<>) a b = WasmSeq a b

instance Monoid (WasmControl s e) where
  mempty = WasmNop

--labelAs :: Labeled a -> b -> Labeled b
--labelAs la b = fmap (const b) la

----------------------------------------------------------------

data FallThroughSet = EmptyFT | AtMost Int

addZeroBump :: FallThroughSet -> FallThroughSet
addZeroBump EmptyFT = AtMost 0
addZeroBump (AtMost k) = AtMost (succ k)



wasmPeepholeOpt :: forall s e . WasmControl s e -> WasmControl s e
wasmPeepholeOpt = removeFinalBrs EmptyFT
  where removeFinalBrs :: FallThroughSet -> WasmControl s e -> WasmControl s e
        -- ^ 1st argument lists every `i` for which `br i` is
        -- equivalent to "fall through" in this context
        --
        -- rewrite rules
        removeFinalBrs fts = tx
            where inner :: WasmControl s e -> WasmControl s e
                  inner = removeFinalBrs (addZeroBump fts)
                  tx (WasmBr tgt)
                      | fts `hasTgt` tgt = WasmNop
                  tx (WasmBlock body) = smartBlock (inner body)
                  tx (WasmLoop body) = WasmLoop (removeFinalBrs (AtMost 0) body)
                  tx (WasmIf e t f) = WasmIf e (inner t) (inner f)
--                  tx (WasmBrIf _ tgt)
--                      | fts `hasTgt` tgt = WasmNop
                  tx (WasmSeq a b) = case tx b of
                                       WasmNop -> tx a
                                       b' -> removeFinalBrs EmptyFT a <> b'
                  tx s = s


        EmptyFT `hasTgt` _ = False
        AtMost n `hasTgt` k = k <= n

        smartBlock WasmNop = WasmNop
        smartBlock s = WasmBlock s
