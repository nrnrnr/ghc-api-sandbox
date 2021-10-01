module Simple.Stg where

import GHC.Core (AltCon)
import GHC.Core.DataCon (DataCon)
import GHC.Stg.Syntax (StgOp, UpdateFlag(..))
import GHC.Types.Var

type Name = Id

-- newtype DataCon = DataCon Name
type Prim = StgOp



data Program = Program [Bind]

data Bind = Bind { lhs :: Name, rhs :: Lambda }

data Lambda = LF { free :: [Name], upd :: UpdateFlag, args :: [Name], lam_body :: Exp }

-- data UpdateFlag = Updatable | ReEntrant | SingleEntry

data Exp = Let    { let_bind :: Bind,    let_body :: Exp } -- allocate closure
         | LetRec { let_binds :: [Bind], let_body :: Exp } -- allocate closures
         | Case   { scrutinee :: Exp, result :: Name, case_alts :: [Alt] } -- force and choose
         | Funcall Name [Atom] -- tail call
         | Construct DataCon [Atom] -- returns
         | Primitive Prim [Atom]
         | Literal Literal

data Atom = Name Name | Lit Literal

data Alt = Alt AltCon [Name] Exp

--data AltCon = DataAlt DataCon
--            | LitAlt Literal
--            | DEFAULT


data Default = Default (Maybe Name) Exp

type Literal = Int
