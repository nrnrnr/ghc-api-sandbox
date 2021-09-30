module Simple.Stg where

import GHC.Types.Name

newtype DataCon = DataCon Name
newtype Prim = Prim Name



data Program = Program [Bind]

data Bind = Bind { lhs :: Name, rhs :: Lambda }

data Lambda = LF { free :: [Name], upd :: UpdateFlag, args :: [Name], lam_body :: Exp }

data UpdateFlag = Updatable | ReEntrant | SingleEntry

data Exp = Let    { let_bind :: Bind,    let_body :: Exp } -- allocate closure
         | LetRec { let_binds :: [Bind], let_body :: Exp } -- allocate closures
         | Case   { scrutinee :: Exp, case_alts :: Alts } -- force and choose
         | Funcall Name [Atom] -- tail call
         | Construct DataCon [Atom] -- returns
         | Primitive Prim [Atom]
         | Literal Literal

data Atom = Name Name | Lit Literal

data Alts = AlgAlts [(Pattern, Exp)] Default

data Pattern = ConPat DataCon [Name]

data Default = Default (Maybe Name) Exp

type Literal = Int
