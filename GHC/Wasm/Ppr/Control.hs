{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Wasm.Ppr.Control
  ( pprStmt
  )
where

import Prelude hiding ((<>))

import GHC.Utils.Outputable
import GHC.Wasm.ControlFlow


data LabeledView a = LV SDoc a

--lview :: Labeled a -> LabeledView a
--lview la = LV (render (labelOf la)) (withoutLabel la)
--  where render Nothing = text "-"
--        render (Just l) = ppr l

lview :: a -> LabeledView a
lview la = LV (text "-") la




instance (OutputableP env s, OutputableP env e) => OutputableP env (WasmControl s e) where
  pdoc = pprStmt

pprStmt :: (OutputableP env s, OutputableP env e) => env -> WasmControl s e -> SDoc

pprStmt _ WasmNop = text "nop"

-- pprStmt _ (WasmComment c) = text "/*" <+> text c <+> text "*/"

pprStmt _ WasmUnreachable = text "unreachable"

pprStmt env (WasmBlock (lview -> LV l body)) =
    text "block" <+> text ";; label =" <+> l $+$
    nest smallindent (pprStmt env body) $+$
    text "end" <+> text ";; block label = " <+> ppr l

pprStmt env (WasmLoop (lview -> LV l body)) =
    text "loop" <+> text ";; label =" <+> l $+$
    nest smallindent (pprStmt env body) $+$
    text "end" <+> text ";; loop label = " <+> l

pprStmt env (WasmIf (lview -> LV _ e) t f) =
    pdoc env e $+$
    text "if" $+$
    nest smallindent (pprStmt env t) $+$
    text "else" $+$
    nest smallindent (pprStmt env f) $+$
    text "end ;; if"

pprStmt _ (WasmBr (lview -> LV l i)) =
    text "br" <+> int i <+> comment (text "to" <+> l)

--pprStmt env (WasmBrIf (lview -> LV _ e) (lview -> LV l i)) =
--    pdoc env e $+$
--    text "br_if" <+> int i <+> comment (text "to" <+> l)

pprStmt env (WasmBrTable (lview -> LV _ e) range targets default') =
    pdoc env e <+> comment (ppr range) $+$
    text "br_table" <+> hsep (map target targets) <+> target default'
  where target (lview -> LV l i) = int i <+> comment l

pprStmt _ WasmReturn = text "return"

pprStmt env (WasmSlc (lview -> LV _ s)) = pdoc env s
pprStmt env (WasmSeq a b) = pprStmt env a $+$ pprStmt env b

--pprStmt _ (WasmLabel lv)
--        | Just l <- labelOf lv = comment (ppr l <> text ":")
--        | otherwise = panic "label statement has no label"



comment :: SDoc -> SDoc -- ^ Wasm block comment
comment l = text "(;" <> l <> text ";)"


smallindent :: Int
smallindent = 2
