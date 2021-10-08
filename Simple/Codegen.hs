module Simple.Codegen where

import qualified Simple.Stg   as S  -- source language
import qualified Simple.Stack as T -- target language

import GHC.Core (AltCon)
import GHC.Core.DataCon (DataCon)
import GHC.Stg.Syntax (StgOp, UpdateFlag(..))
import GHC.Types.Var
import GHC.Types.Unique.Supply

{-
[Note register allocation]

This code generator never tries to reuse a register.  It assumes that
reusing registers may make it harder for a downstream optimizer to map
virtual registers onto real machine resources.
-}



type Gen a = UniqSM a -- needs stack thingy too?

-- |What to do with the value of an expression
data Context = Reg T.Register | Return

expr :: T.Register -> S.Exp -> T.Code -> T.Code
expr dest e k =
  case e of
    S.Literal v -> assign $ T.Literal v
    S.Primitive p args -> assign $ T.Primitive p args
    S.Construct c args -> assign $ T.Construct c args

  where assign sle = T.Assign dest sle k

{-

type Name = Id

-- newtype DataCon = DataCon Name
type Prim = StgOp



data Program = Program [Bind]

data Bind = Bind { lhs :: Name, rhs :: Rhs }

data Rhs = Lambda { free :: [Name], upd :: UpdateFlag, args :: [Name], lam_body :: Exp }
         | RhsCon DataCon [Atom]

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

-}
