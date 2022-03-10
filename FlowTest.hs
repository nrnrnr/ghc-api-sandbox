module FlowTest
  ( InterpTest(..)
  , cmmPathResults
  , tracesMatch
  , outputTraceContinues

  , wasmResults


  )
where

import Data.List

import GHC.Cmm
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.ControlFlow.Run
import GHC.Test.CmmPaths
import GHC.Test.ControlMonad
import GHC.Wasm.ControlFlow
import GHC.Wasm.ControlFlow.Run

type Trace stmt exp = [Event stmt exp]

data InterpTest stmt exp a = IT { it_input :: a, it_output :: FinalState stmt exp () }
  deriving (Show)

cmmPathResults :: CmmGraph -> [InterpTest Label Label (Trace Label Label)]
cmmPathResults g = [IT input (reverseEvents $ runWithBits (evalGraph s e g) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g
        s = entryLabel
        e = entryLabel

tracesMatch :: (Eq stmt, Eq exp) => InterpTest stmt exp (Trace stmt exp) -> Bool
tracesMatch it = it_input it == pastEvents (it_output it)

-- | The input trace comes to an end, but the output trace keeps going,
-- perhaps until entropy is exhausted
outputTraceContinues :: (Eq e, Eq s) => InterpTest s e (Trace s e) -> Bool
outputTraceContinues it = it_input it `isPrefixOf` pastEvents (it_output it)

----------------------------------------------------------------

wasmResults :: CmmGraph -> WasmStmt s e -> [InterpTest Label Label (Trace Label Label)]
wasmResults g w = [IT input (reverseEvents $ runWithBits (evalWasm w) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g

-- wasmPeepholeResults :: Platform -> CmmGraph -> [InterpTest Trace]
-- wasmPeepholeResults platform g =
--     [IT input (reverseEvents $ runWithBits (evalWasm w) (traceBits input))
--                         | input <- traces ]
--   where traces = eventPaths g
--         w = wasmPeepholeOpt $ structuredControl platform  (\l _ -> l) (\l _ -> l) g
