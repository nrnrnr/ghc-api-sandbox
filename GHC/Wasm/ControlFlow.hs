{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Wasm.ControlFlow 
  ( WasmStmt(..)
  , Labeled, pattern Labeled
  , BranchTyped(..), BranchType(..)
  , pattern WasmExit, pattern WasmContinue

  , wasmLabeled

  , wasmPeepholeOpt
  , wasmControlFaults
  )
where

import Data.Void

import GHC.Cmm.Dataflow.Label (Label)
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Models the control-flow portion of the WebAssembly instruction set

data Labeled a = Labeled' Label a  -- See [Note labels]

pattern Labeled :: Label -> a -> Labeled a
pattern Labeled l a = Labeled' l a

{-# COMPLETE Labeled #-}

-- [Note labels]
--
-- The representation includes labels, which are completely redundant.
-- They exist only for sanity checking.  The `Labeled` type is kept
-- abstract, so that if we want to eliminate labels from the representation,
-- clients won't have to change.






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


data BranchType = ExitBranch | ContinueBranch -- for sanity checking only

data BranchTyped a = BranchTyped BranchType a

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

  WasmBlock :: Labeled (WasmStmt s e) -> WasmStmt s e
  WasmLoop  :: Labeled (WasmStmt s e) -> WasmStmt s e
  WasmIf    :: Labeled e -> WasmStmt s e -> WasmStmt s e -> WasmStmt s e

  WasmBr   :: BranchTyped (Labeled Int) -> WasmStmt s e
  WasmBrIf :: e -> BranchTyped (Labeled Int) -> WasmStmt s e
  WasmBrTable :: [Labeled Int] -> Labeled Int -> WasmStmt s e
  WasmReturn :: WasmStmt s e

  WasmSlc :: s -> WasmStmt s e   -- straight-line code
  WasmSeq :: WasmStmt s e -> WasmStmt s e -> WasmStmt s e

  WasmLabel :: Labeled Void -> WasmStmt s e -- pure sanity-checking play

pattern WasmExit :: Label -> Int -> WasmStmt s e
pattern WasmContinue :: Label -> Int -> WasmStmt s e

pattern WasmExit     l i = WasmBr (BranchTyped ExitBranch     (Labeled l i))
pattern WasmContinue l i = WasmBr (BranchTyped ContinueBranch (Labeled l i))

wasmLabeled :: Label -> (Labeled a -> b) -> a -> b
wasmLabeled l c a = c (Labeled l a)



instance Semigroup (WasmStmt s e) where
  (<>) = WasmSeq

instance Monoid (WasmStmt s e) where
  mempty = WasmNop


wasmPeepholeOpt :: WasmStmt s e -> WasmStmt s e
wasmPeepholeOpt _ = panic "peephole optimizer not implemented"

wasmControlFaults :: WasmStmt s e -> Maybe SDoc
wasmControlFaults _ = panic "fault checking not implemented"
