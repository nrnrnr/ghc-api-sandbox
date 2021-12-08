{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Wasm.Ppr.Control
  ( pprStmt
  )
where

import Prelude hiding ((<>))

import GHC.Utils.Outputable
import GHC.Wasm.ControlFlow



instance (OutputableP env s, OutputableP env e) => OutputableP env (WasmStmt s e) where
  pdoc = pprStmt

pprStmt :: (OutputableP env s, OutputableP env e) => env -> WasmStmt s e -> SDoc

pprStmt _ WasmNop = text "nop"

pprStmt _ WasmUnreachable = text "unreachable"

pprStmt env (WasmBlock (Labeled l body)) =
    text "block" <+> text ";; label =" <+> ppr l $+$
    nest smallindent (pprStmt env body) $+$
    text "end" <+> text ";; block label = " <+> ppr l

pprStmt env (WasmLoop (Labeled l body)) =
    text "loop" <+> text ";; label =" <+> ppr l $+$
    nest smallindent (pprStmt env body) $+$
    text "end" <+> text ";; loop label = " <+> ppr l

pprStmt env (WasmIf (Labeled _ e) t f) =
    pdoc env e $+$
    text "if" $+$
    nest smallindent (pprStmt env t) $+$
    text "else" $+$
    nest smallindent (pprStmt env f) $+$
    text "end ;; if"

pprStmt _ (WasmBr (BranchTyped ty (Labeled l i))) =
    text "br" <+> int i <+> comment (ppr ty <+> text "to" <+> ppr l)

pprStmt env (WasmBrIf e (BranchTyped ty (Labeled l i))) =
    pdoc env e $+$
    text "br_if" <+> int i <+> comment (ppr ty <+> text "to" <+> ppr l)

pprStmt env (WasmBrTable e targets default') =
    pdoc env e $+$
    text "br_table" <+> hsep (map target targets) <+> target default'
  where target (Labeled l i) = int i <+> comment (ppr l)

pprStmt _ WasmReturn = text "return"

pprStmt env (WasmSlc s) = pdoc env s
pprStmt env (WasmSeq a b) = pprStmt env a $+$ pprStmt env b

pprStmt _ (WasmLabel (Labeled l _)) = comment (ppr l <> text ":")


instance Outputable BranchType where
  ppr ExitBranch = text "exit"
  ppr ContinueBranch = text "continue"

comment :: SDoc -> SDoc -- ^ Wasm block comment
comment l = text "(;" <> l <> text ";)"


smallindent :: Int
smallindent = 2
