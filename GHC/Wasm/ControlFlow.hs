{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow 
  ( WasmStmt(..)
  , Labeled, pattern Labeled, withoutLabel, labelOf
  , BranchTyped(..), BranchType(..)
  , BrTableInterval(..), inclusiveInterval
  , pattern WasmExit, pattern WasmContinue

  , wasmLabeled
  , wasmUnlabeled

  , wasmPeepholeOpt
  , wasmControlFaults
  )
where

import Data.Void

--import Debug.Trace

import GHC.Cmm.Dataflow.Label (Label)
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

-- | Models the control-flow portion of the WebAssembly instruction set

data Labeled a = Labeled { _lb_label :: Label, withoutLabel :: a }  -- See [Note labels]
               | Unlabeled { withoutLabel :: a }

labelOf :: Labeled a -> Maybe Label
labelOf (Labeled l _) = Just l
labelOf (Unlabeled _) = Nothing

instance Functor Labeled where
  fmap f (Labeled l a) = Labeled l (f a)
  fmap f (Unlabeled a) = Unlabeled (f a)

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
  WasmBrIf :: Labeled e -> BranchTyped (Labeled Int) -> WasmStmt s e
  WasmBrTable :: Labeled e
              -> BrTableInterval -- for debugging only
              -> [Labeled Int]
              -> Labeled Int
              -> WasmStmt s e
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturn :: WasmStmt s e

  WasmSlc :: Labeled s -> WasmStmt s e   -- straight-line code
  WasmSeq :: WasmStmt s e -> WasmStmt s e -> WasmStmt s e

  WasmLabel :: Labeled Void -> WasmStmt s e -- pure sanity-checking play

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

pattern WasmExit :: Label -> Int -> WasmStmt s e
pattern WasmContinue :: Label -> Int -> WasmStmt s e

pattern WasmExit     l i = WasmBr (BranchTyped ExitBranch     (Labeled l i))
pattern WasmContinue l i = WasmBr (BranchTyped ContinueBranch (Labeled l i))

wasmLabeled :: Label -> (Labeled a -> b) -> a -> b
wasmLabeled l c a = c (Labeled l a)

wasmUnlabeled :: (Labeled a -> b) -> a -> b
wasmUnlabeled c a = c (Unlabeled a)

instance Semigroup (WasmStmt s e) where
  (<>) WasmNop a = a
  (<>) a WasmNop = a
  (<>) a b = WasmSeq a b

instance Monoid (WasmStmt s e) where
  mempty = WasmNop

--labelAs :: Labeled a -> b -> Labeled b
--labelAs la b = fmap (const b) la

data FallThroughSet = EmptyFT | AtMost Int

addZeroBump :: FallThroughSet -> FallThroughSet
addZeroBump EmptyFT = AtMost 0
addZeroBump (AtMost k) = AtMost (succ k)

wasmPeepholeOpt :: forall s e . WasmStmt s e -> WasmStmt s e
wasmPeepholeOpt = removeFinalBrs EmptyFT
  where removeFinalBrs :: FallThroughSet -> WasmStmt s e -> WasmStmt s e
        -- ^ 1st argument lists every `i` for which `br i` is
        -- equivalent to "fall through" in this context
        -- 
        -- rewrite rules
        removeFinalBrs fts = tx
            where inner :: WasmStmt s e -> WasmStmt s e
                  inner = removeFinalBrs (addZeroBump fts)
                  tx (WasmBr (BranchTyped _ tgt))
                      | fts `hasTgt` (withoutLabel tgt) = WasmNop
                  tx (WasmBlock body) = smartBlock (fmap inner body)
                  tx (WasmLoop body) = WasmLoop (fmap (removeFinalBrs (AtMost 0)) body)
                  tx (WasmIf e t f) = WasmIf e (inner t) (inner f)
                  tx (WasmBrIf _ (BranchTyped _ tgt))
                      | fts `hasTgt` (withoutLabel tgt) = WasmNop
                  tx (WasmSeq a b) = case tx b of
                                       WasmNop -> tx a
                                       b' -> removeFinalBrs EmptyFT a <> b'
                  tx s = s
                  

        EmptyFT `hasTgt` _ = False
        AtMost n `hasTgt` k = k <= n

        smartBlock s | WasmNop <- withoutLabel s = WasmNop
        smartBlock s = WasmBlock s

{-
        smartBlock c lbl (viewSnoc -> (s, WasmBr (BranchTyped _t tgt)))
            | k == 0 = smartBlock c lbl s -- good opt
--            | otherwise = smartBlock c lbl s <> WasmBr (BranchTyped t (fmap pred tgt)) -- bad
          where k = withoutLabel tgt
        smartBlock c lbl s = c (lbl s)

           -- GOOD: block s; br 0 end --> block s end
           -- BAD: block s; br (m + 1) end --> block s end; br m

           -- To come: block; nop; end --> nop

        bad (WasmBlock ls) = smartBlock WasmBlock (labelAs ls) (bad $ withoutLabel ls)
        bad (WasmLoop ls)  = smartBlock WasmLoop (labelAs ls) (bad $ withoutLabel ls)
        bad (WasmIf e t f) = WasmIf e (bad t) (bad f)
        bad (WasmSeq s s') = WasmSeq (bad s) (bad s')
        bad s = s

        _viz tgt = case labelOf tgt of Nothing -> " (unlabeled)"
                                       Just l -> " br to " ++ showSDocUnsafe (ppr l)

viewSnoc :: WasmStmt s e -> (WasmStmt s e, WasmStmt s e)
viewSnoc (WasmSeq a (WasmSeq b c)) = viewSnoc (WasmSeq a b `WasmSeq` c)
viewSnoc (WasmSeq a b) = (a, b)
viewSnoc s = (WasmNop, s)




-}


wasmControlFaults :: WasmStmt s e -> Maybe SDoc
wasmControlFaults _ = panic "fault checking not implemented"



