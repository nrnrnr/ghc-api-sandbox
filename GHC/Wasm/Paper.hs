{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.Paper
  ( WasmStmt(..)
  , BrTableInterval(..), inclusiveInterval
  , pattern WasmExit, pattern WasmContinue

  )
where

import Data.Void

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


data WasmStmt s e where
  -- ^ Type parameter `s` is the type of (unspecified) statements.
  -- It might be instantiated with an open Cmm block or with a sequence
  -- of Wasm instructions.
  --
  -- Type parameter `e` is the type of conditional expressions.
  -- It might be instantiated with a Cmm expression or with a
  -- sequence of Wasm instructions that would leave a value of
  -- interest on top of the stack.

  WasmNop :: WasmStmt s e

  WasmBlock :: WasmStmt s e -> WasmStmt s e
  WasmLoop  :: WasmStmt s e -> WasmStmt s e
  WasmIf    :: e -> WasmStmt s e -> WasmStmt s e -> WasmStmt s e

  WasmBr   :: Int -> WasmStmt s e
  WasmBrIf :: e -> Int -> WasmStmt s e
  WasmBrTable :: e
              -> BrTableInterval -- for debugging only
              -> [Int]
              -> Int
              -> WasmStmt s e
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturn :: WasmStmt s e

  WasmSlc :: s -> WasmStmt s e   -- straight-line code
  WasmSeq :: WasmStmt s e -> WasmStmt s e -> WasmStmt s e

  WasmLabel :: Void -> WasmStmt s e -- pure sanity-checking play

  WasmUnreachable :: WasmStmt s e


data BrTableInterval
    = BrTableInterval { bti_lo :: Integer, bti_count :: Integer }

instance Outputable BrTableInterval where
  ppr range = brackets $ hcat[integer (bti_lo range), text "..", integer hi]
      where hi = bti_lo range + bti_count range - 1

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = BrTableInterval lo (hi - lo + 1)
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

pattern WasmExit :: Int -> WasmStmt s e
pattern WasmContinue :: Int -> WasmStmt s e

pattern WasmExit     i = WasmBr i
pattern WasmContinue i = WasmBr i

instance Semigroup (WasmStmt s e) where
  (<>) WasmNop a = a
  (<>) a WasmNop = a
  (<>) a b = WasmSeq a b

instance Monoid (WasmStmt s e) where
  mempty = WasmNop

--labelAs :: Labeled a -> b -> Labeled b
--labelAs la b = fmap (const b) la


