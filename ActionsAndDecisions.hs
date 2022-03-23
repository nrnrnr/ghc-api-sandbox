{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ActionsAndDecisions
  ( Stmt, Expr
  , stmt, expr
  )
where

import GHC.Utils.Outputable

import GHC.Cmm
import GHC.Cmm.Ppr ()
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Platform

data Stmt = Stmt { s_label :: Label
                 , _s_body :: Block CmmNode O O
                 , s_rendering :: String
                 }

data Expr = Expr { e_label :: Label
                 , _e_exp :: CmmExpr
                 , e_rendering :: String
                 }

stmt :: Label -> Block CmmNode O O -> Stmt
stmt lbl body = Stmt lbl body (showSDocUnsafe $ pdoc genericPlatform $ body)

expr :: Label -> CmmExpr -> Expr
expr lbl e = Expr lbl e (showSDocUnsafe $ pdoc genericPlatform $ e)

instance Eq Stmt where
  s == s' = s_label s == s_label s' || s_rendering s == s_rendering s'

instance Eq Expr where
  e == e' = e_label e == e_label e' || e_rendering e == e_rendering e'

instance Show Stmt where
  show = showSDocUnsafe . ppr . s_label

instance Show Expr where
  show = showSDocUnsafe . ppr . e_label
