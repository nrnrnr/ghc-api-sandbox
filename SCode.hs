{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SCode
  ( SCode(..)
  , Code(..)
  )
where

import Prelude hiding ((<>))

import Data.Kind

import qualified GHC.Base as B

import GHC.Cmm
import GHC.Cmm.Dataflow.Label
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

  repeatx l body = S $ text "repeat" <+> ppr l $+$
                       nest smallindent (unS body) $$
                       text "end repeat" <+> ppr l

  block l body = S $ text "block" <+> ppr l $+$
                       nest smallindent (unS body) $$
                       text "end block" <+> ppr l

  ifx c l t f =  S $ text "if" <+> pprExpr genericPlatform c <+> text "then" $$
                     nest smallindent (unS t) $$
                     text "else" $$
                     nest smallindent (unS f) $$
                     text "end if" <+> ppr l

  goto l i = S $ text "exit" <+> int i <+> text "(goto" <+> ppr l <> text ")"

  fallThrough l = S $ text "-- fall through to" <+> ppr l

  continue l i = S $ text "continue" <+> int i <+> text "(goto" <+> ppr l <> text ")"
  failedContinue l doc =
      S $ text "continue FAILED" <+> text "(goto" <+> ppr l <> text ")" <+> doc

  gotoExit = S $ text "return"

  codeBody block = S $ nest bigindent $ pdoc genericPlatform block

  switch e targets other = S $
    text "switch" <+> parens (pprExpr genericPlatform e) $$
    nest smallindent (
      vcat (zipWith knownCase targets [0..]) $$
      unknownCase other) $$
    text "end switch"
   where knownCase (lbl, tnum) i = text "case" <+> int i <> text ":" <+> rhs lbl tnum
         rhs lbl tnum = text "br" <+> int tnum <+> text "(goto" <+> ppr lbl <> text ")"
         unknownCase (lbl, tnum) = text "default:" <+> rhs lbl tnum              
          


class (Monoid c) => Code c where
  type CodeExpr c :: Type
  -- c is a statement
  -- CodeExpr c is an expression, such as might appear as an `if` condition
  codeLabel :: Label -> c

  repeatx :: Label -> c -> c

  ifx :: CodeExpr c -> Label -> c -> c -> c --

  block :: Label -> c -> c  -- ^ put code in block so `goto` can be replace with `exit`

  goto        :: Label -> Int -> c  -- ^ exit; translates as `br k`
  fallThrough :: Label -> c  -- ^ generates no code; used to help debug
  continue :: Label -> Int -> c  -- ^ restart loop; translates as `br k`

  failedContinue :: Label -> SDoc -> c -- ^ tried to continue but there's no target on the stack

  switch :: CodeExpr c -> [(Label, Int)] -> (Label, Int) -> c


  gotoExit :: c -- ^ stop the function (return or tail call)

  codeBody :: CmmBlock -> c -- ^ straight-line code

