module FlowTest
  ( InterpTest(..)
  , cmmPathResults
  , tracesMatch
  , outputTraceContinues

  , wasmPathResults

  )
where

import Data.List

import GHC.Cmm
import GHC.Cmm.ControlFlow.Run
import GHC.Platform
import GHC.Test.CmmPaths
import GHC.Test.ControlMonad
import GHC.Wasm.ControlFlow.OfCmm
import GHC.Wasm.ControlFlow.Run

type Trace = [Event]

data InterpTest a = IT { it_input :: a, it_output :: FinalState () }
  deriving (Show)

cmmPathResults :: CmmGraph -> [InterpTest Trace]
cmmPathResults g = [IT input (reverseEvents $ runWithBits (evalGraph g) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g

tracesMatch :: InterpTest Trace -> Bool
tracesMatch it = it_input it == pastEvents (it_output it)

-- | The input trace comes to an end, but the output trace keeps going,
-- perhaps until entropy is exhausted
outputTraceContinues :: InterpTest Trace -> Bool
outputTraceContinues it = it_input it `isPrefixOf` pastEvents (it_output it)

----------------------------------------------------------------

wasmPathResults :: Platform -> CmmGraph -> [InterpTest Trace]
wasmPathResults platform g = [IT input (reverseEvents $ runWithBits (evalWasm w) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g
        w = structuredControl platform  (\l _ -> l) (\l _ -> l) g
