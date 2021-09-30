module Simple.Stack where

import GHC.Types.Name (Name)

newtype DataCon = DataCon Name
newtype Prim = Prim Name

newtype Register = Register Name


data Program = Program [Bind Name]

data Bind name = Bind { lhs :: name, rhs :: Lambda }

data Lambda = LF { free :: [Name], upd :: UpdateFlag, lam_args :: [Name], code :: Function }

data UpdateFlag = Updatable | ReEntrant | SingleEntry

data Function = Function { frame_layout :: Frame, fun_body :: Code }

data Slot = Slot Name Metrics -- identifies a slot in the stack frame

data Metrics = Metrics { met_size :: Int, met_alignment :: Int }
  -- size and alignment in bytes

data Frame = Frame [Slot]

data Code
  = Spill Slot Register Code
  | Reload Register Slot Code
  | Let    { let_bind  :: Bind Register,   let_body :: Code } -- allocate closure
  | LetRec { let_binds :: [Bind Register], let_body :: Code } -- allocate closures
  | Case   { scrutinee :: Register
           , case_alts :: Alts
           , cont :: Cont }
  | Funcall { result :: Register -- evaluation, may require spills
            , forcity :: Forcity
            , fun :: Name
            , args :: [Atom]
            , cont :: Cont
            } -- non-tail call
  | Tailcall { fun :: Name
             , args :: [Atom]
             }
  | Assign Register SLE Code
  | Return Register

-- design question: should Code carry a list of live registers with it?


data SLE -- straight-line expression: no calls, allocation, or case expressions
  = Construct DataCon [Atom] -- returns
  | Primitive Prim [Atom]
  | Literal Literal

data Cont -- continuation
  = Cont Code
  | Unreachable -- continuation of a case expression whose alts all end in tail calls?

data Forcity
  = Forced -- appears as scrutinee of case expression or other context where
           -- we know statically that the *result* will be evaluated
  | Unforced -- result of call might not be forced

data Atom = Name Name | Lit Literal

data Alts = AlgAlts [(Pattern, Code)] Default

data Pattern = ConPat DataCon [Name]

data Default = Default (Maybe Name) Code

type Literal = Int
