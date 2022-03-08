module Simple.Common (
  Atom(..), Name
) where

import GHC.Types.Var(Id)


type Name = Id

data Atom = Name Name | Lit Literal
type Literal = Int
