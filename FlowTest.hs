module FlowTest
  ( InterpTest(..)
  , cmmPathResults
  , tracesMatch
  , outputTraceContinues
  )
where

import Data.List

import GHC.Cmm
import GHC.Cmm.ControlFlow.Run
import GHC.Test.CmmPaths
import GHC.Test.ControlMonad

type Trace = [Event]

data InterpTest = IT { it_input :: Trace, it_output :: FinalState () }
  deriving (Show)

cmmPathResults :: CmmGraph -> [InterpTest]
cmmPathResults g = [IT input (reverseEvents $ runWithBits (evalGraph g) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g

tracesMatch :: InterpTest -> Bool
tracesMatch it = it_input it == pastEvents (it_output it)

outputTraceContinues :: InterpTest -> Bool
outputTraceContinues it = it_input it `isPrefixOf` pastEvents (it_output it)
