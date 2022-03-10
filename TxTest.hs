{-# OPTIONS_GHC -Wno-unused-imports #-}

module TxTest
where

import Prelude hiding ((<>))

import Words

import Crypto.Hash.SHA1

import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Maybe
import Data.List hiding (foldl)

import DotCfg
import DotGraph

import FlowTest
import GHC.Test.ControlMonad
import GHC.Types.Unique.Supply

import Control.Monad
import Control.Monad.IO.Class

import Attic.UnoptControlTx
import qualified GHC.Wasm.ControlFlow.FromCmm as Opt

import GHC.Driver.Config.StgToCmm (initStgToCmmConfig)

import GHC.Wasm.ControlFlow

import GHC.Cmm.Dominators

--import Debug.Trace


import System.FilePath as FilePath

import StgToCmmLite (codeGen)


import GHC
import GHC.Core.TyCon
import GHC.CoreToStg
import GHC.CoreToStg.Prep
import GHC.Data.Stream hiding (mapM, map)
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Platform
import GHC.Stg.Syntax
import GHC.Stg.FVs
import GHC.Types.IPE (emptyInfoTableProvMap)
import GHC.Types.CostCentre (emptyCollectedCCs)
import GHC.Types.HpcInfo (emptyHpcInfo)
import GHC.Unit.Home
import GHC.Utils.Misc (fstOf3)
import GHC.Unit.Module.ModGuts
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Ppr (Mode(PageMode))

import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dominators.Lint
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Parser
import GHC.Cmm.Ppr()

import qualified GHC.LanguageExtensions as LangExt

import System.Environment ( getArgs )
import System.IO (stdout, stderr, hPutStrLn, hFlush)

--import GHC.Wasm.ControlFlow
import GHC.Wasm.Ppr.Control()



data Language a event =
    Language { lang_text :: a -> SDoc
             , lang_dot  :: Maybe (a -> SDoc)
             , lang_paths :: a -> [[event]]
             }
txTest :: Language a event -> Language b event -> [event] -> a -> b -> TestResult
txTest = undefined

analyzeTest :: InterpTest [Event Label Label] -> (TestResult, IO ())
analyzeTest t =
    if tracesMatch t then
        (Good, putStrLn $ "EXACT: " ++ show (it_input t))
    else if outputTraceContinues t then
        (Good, putStrLn $ "CONTINUES: " ++ show (it_output t))
    else
        ( Bad
        , do putStrLn $ "NO MATCH:"
             putStrLn $ "  " ++ show (it_input t)
             putStrLn $ "  " ++ show (it_output t)
             putStrLn $ "Differ in position " ++ diffPos t
        )
  where diffPos t = badIndex (0::Int) (it_input t) (pastEvents (it_output t))
        badIndex k [] [] = "PERFECT MATCH at " ++ show k
        badIndex k (e:es) (e':es')
           | e == e' = badIndex (k+1) es es'
           | otherwise = show k ++ " (" ++ show e ++ " vs " ++ show e' ++ ")"
        badIndex k [] (_:_) = show k ++ " (input runs out first)"
        badIndex k (_:_) [] = show k ++ " (output runs out first)"



data TestResult = Good | Bad
  deriving Eq
