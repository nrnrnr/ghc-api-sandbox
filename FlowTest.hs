{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FlowTest
  ( InterpTest(..)
  , cmmPathResults
  , tracesMatch
  , outputTraceContinues

  , wasmResults

  , Stmt(..), Exp(..)

  )
where

import Data.Function

import GHC.Utils.Outputable

import GHC.Cmm
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.ControlFlow.Run
import GHC.Test.CmmPaths
import GHC.Test.ControlMonad
import GHC.Wasm.ControlFlow
import GHC.Wasm.ControlFlow.Run

type Trace stmt exp = [Event stmt exp]

data Stmt = Stmt { s_label :: Label
                 , s_body :: Block CmmNode O O
                 }

data Exp = Exp { e_label :: Label
               , e_exp :: CmmExpr
               }

instance Eq Stmt where
  (==) = (==) `on` s_label

instance Eq Exp where
  (==) = (==) `on` e_label

instance Show Stmt where
  show = showSDocUnsafe . ppr . s_label

instance Show Exp where
  show = showSDocUnsafe . ppr . e_label


data InterpTest stmt exp a = IT { it_input :: a, it_output :: FinalState stmt exp () }
  deriving (Show)

instance PathTrackable CmmNode (Event Stmt Exp) where
  blockBodyEvent lbl block = Action $ Stmt lbl middle
    where (_, middle, _) = blockSplit block
  blockExitEvents _ block = map tag $ cmmExits block
   where tag (maybeEvent, lbl) = (fmap (eventMap id (uncurry Exp)) maybeEvent, lbl)

cmmPathResults :: CmmGraph
               -> [InterpTest Stmt Exp (Trace Stmt Exp)]
cmmPathResults g =
    [ IT input (reverseEvents $ runWithBits (evalGraph Stmt Exp g) (traceBits input))
    | input <- traces
    ]
  where traces = eventPaths g

tracesMatch :: (Eq stmt, Eq exp)
            => InterpTest stmt exp (Trace stmt exp)
            -> Bool
tracesMatch it = and $ zipWith (eventsMatch (==) (==)) (it_input it) (pastEvents (it_output it))

-- | The input trace comes to an end, but the output trace keeps going,
-- perhaps until entropy is exhausted
outputTraceContinues :: (Eq s, Eq e)
                     => InterpTest s e (Trace s e)
                     -> Bool
outputTraceContinues it = it_input it `isPrefixOf` pastEvents (it_output it)
  where isPrefixOf [] _         =  True
        isPrefixOf _  []        =  False
        isPrefixOf (x:xs) (y:ys)=  match x y && isPrefixOf xs ys
        match = eventsMatch (==) (==)


----------------------------------------------------------------

wasmResults :: PathTrackable CmmNode (Event s e)
            => CmmGraph
            -> WasmControl s e
            -> [InterpTest s e (Trace s e)]
wasmResults g w = [IT input (reverseEvents $ runWithBits (evalWasm w) (traceBits input))
                        | input <- traces ]
  where traces = eventPaths g

-- wasmPeepholeResults :: Platform -> CmmGraph -> [InterpTest Trace]
-- wasmPeepholeResults platform g =
--     [IT input (reverseEvents $ runWithBits (evalWasm w) (traceBits input))
--                         | input <- traces ]
--   where traces = eventPaths g
--         w = wasmPeepholeOpt $ structuredControl platform  (\l _ -> l) (\l _ -> l) g
