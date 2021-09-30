module Simple.Stg where

import GHC.Types.Name

newtype DataCon = DataCon Name
newtype Prim = Prim Name



data Program = Program [Bind]

data Bind = Bind { lhs :: Name, rhs :: Lambda }

data Lambda = LF { free :: [Name], upd :: UpdateFlag, args :: [Name], lam_body :: Exp }

data UpdateFlag = Updatable | ReEntrant | SingleEntry

data Exp = Let    { let_bind :: Bind,    let_body :: Exp }
         | LetRec { let_binds :: [Bind], let_body :: Exp }
         | Case   { scrutinee :: Exp, case_alts :: Alts }
         | Funcall Name [Atom]
         | Construct DataCon [Atom]
         | Primitive Prim [Atom]
         | Literal Literal

data Atom = Name Name | Lit Literal

data Alts = AlgAlts [(Pattern, Exp)] Default

data Pattern = ConPat DataCon [Name]

data Default = Default (Maybe Name) Exp

type Literal = Int
