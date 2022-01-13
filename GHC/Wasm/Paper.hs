{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.Paper
  ( WasmControl(..)
  , BrTableInterval(..), inclusiveInterval
  , pattern WasmExit, pattern WasmContinue

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


data WasmControl s where
  -- ^ Type parameter `s` is the type of (unspecified) statements.
  -- It might be instantiated with an open Cmm block or with a sequence
  -- of Wasm instructions.

  WasmBlock :: WasmControl s -> WasmControl s
  WasmLoop  :: WasmControl s -> WasmControl s
  WasmIf    :: s -> WasmControl s -> WasmControl s -> WasmControl s

  WasmBr   :: Int -> WasmControl s
  WasmBrTable :: s
              -> BrTableInterval -- for debugging only
              -> [Int]
              -> Int
              -> WasmControl s
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturn :: WasmControl s

  WasmSlc :: s -> WasmControl s   -- straight-line code
  WasmSeq :: WasmControl s -> WasmControl s -> WasmControl s


data BrTableInterval
    = BrTableInterval { bti_lo :: Integer, bti_count :: Integer }

instance Outputable BrTableInterval where
  ppr range = brackets $ hcat[integer (bti_lo range), text "..", integer hi]
      where hi = bti_lo range + bti_count range - 1

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = BrTableInterval lo (hi - lo + 1)
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

pattern WasmExit :: Int -> WasmControl s
pattern WasmContinue :: Int -> WasmControl s

pattern WasmExit     i = WasmBr i
pattern WasmContinue i = WasmBr i

instance Semigroup (WasmControl s) where
--  (<>) WasmNop a = a
--  (<>) a WasmNop = a
  (<>) a b = WasmSeq a b

--instance Monoid (WasmControl s) where
--  mempty = WasmNop

--labelAs :: Labeled a -> b -> Labeled b
--labelAs la b = fmap (const b) la


