{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SCode
  ( SCode(..)
  )
where

import PetersonR

import Prelude hiding ((<>))

import qualified GHC.Base as B

import GHC.Cmm
import GHC.Cmm.Ppr
import GHC.Platform
import GHC.Utils.Outputable


newtype SCode = S {unS :: SDoc}

instance Semigroup SCode where
  ((<>)) (S a) (S b) = S (a $$ b)

instance Monoid SCode where
  mempty = S empty

instance Code SCode where
  type instance CodeExpr SCode = CmmExpr
  codeLabel l = S $ text "label" <> space <> ppr l <> text ": "

  repeatx l body = S $ text "repeat" <> space <> ppr l $$
                       nest 2 (unS body) $$
                       text "end" <> space <> ppr l

  block l body = S $ text "block" <> space <> ppr l $$
                       nest 2 (unS body) $$
                       text "end" <> space <> ppr l

  ifx c l t f =  S $ (unS (codeLabel l)) $$
                     text "if" <> space <> pprExpr genericPlatform c <> space <> text "then" $$
                     nest 2 (unS t) $$
                     text "else" $$
                     nest 2 (unS f) $$
                     text "end" <> space <> ppr l

  goto l i = S $ text "exit" <> space <> int i <>
                 space <> text "(goto " <> ppr l <> text ")"
  fallThrough l = S $ text "-- fall through to" <+> ppr l

  continue l i = S $ text "continue" <> space <> int i <>
                 space <> text "(goto " <> ppr l <> text ")"

  gotoExit = S $ text "return"

  codeBody block = S $ nest 4 $ pdoc genericPlatform block
