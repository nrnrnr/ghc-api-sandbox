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

smallindent :: Int
bigindent :: Int
smallindent = 4
bigindent = 8


instance Code SCode where
  type instance CodeExpr SCode = CmmExpr
  codeLabel l = S $ text "label" <+> ppr l <> text ": "

  repeatx l body = S $ text "repeat" <+> ppr l $$
                       nest smallindent (unS body) $$
                       text "end repeat" <+> ppr l

  block l body = S $ text "block" <+> ppr l $$
                       nest smallindent (unS body) $$
                       text "end block" <+> ppr l

  ifx c l t f =  S $ (unS (codeLabel l)) $$
                     text "if" <+> pprExpr genericPlatform c <+> text "then" $$
                     nest smallindent (unS t) $$
                     text "else" $$
                     nest smallindent (unS f) $$
                     text "end if" <+> ppr l

  goto l i = S $ text "exit" <+> int i <+> text "(goto " <> ppr l <> text ")"

  fallThrough l = S $ text "-- fall through to" <+> ppr l

  continue l i = S $ text "continue" <+> int i <+> text "(goto " <> ppr l <> text ")"

  gotoExit = S $ text "return"

  codeBody block = S $ nest bigindent $ pdoc genericPlatform block
