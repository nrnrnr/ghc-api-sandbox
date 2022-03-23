{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import System.Environment ( getArgs )
import Control.Monad.IO.Class

import GHC hiding (Stmt)
import GHC.Cmm
import GHC.Cmm.Dominators
import GHC.Cmm.Reducibility
import GHC.Driver.Session
import GHC.Types.Unique.Supply

import qualified GHC.LanguageExtensions as LangExt

import ActionsAndDecisions
import BitConsumer
import ControlTestMonad
import LoadCmmGroup
import RunCmm

main :: IO ()
main = do
    libdir:files <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        raw_dflags <- getSessionDynFlags
        let dflags = raw_dflags `xopt_set` LangExt.MagicHash
                         `xopt_set` LangExt.StandaloneKindSignatures
                         `xopt_set` LangExt.UnliftedDatatypes
                         `xopt_set` LangExt.DataKinds
        setSessionDynFlags dflags
        -- let sdctx = initSDocContext dflags defaultUserStyle
        groups <- mapM loadPath files
        liftIO $ putStrLn $ "loaded " ++ count groups "group"

count :: [a] -> String -> String
count xs thing = case length xs of
                   1 -> "1 " ++ thing
                   n -> show n ++ " " ++ thing ++ "s"


data Outcome = Identical { npaths :: Int }
             | Different { different :: [(Trace, Trace)], nsame :: Int }
type Trace = [Event Stmt Expr]

testGraphReduction :: CmmGraph -> IO Outcome
testGraphReduction original_graph = do
  reducible_graph <- fmap gwd_graph $ runUniqSM $
                     asReducible $ graphWithDominators original_graph
  return $ case reducibility (graphWithDominators original_graph) of
    Reducible -> Identical 0
    Irreducible ->
      compareWithEntropy (runcfg original_graph) (runcfg reducible_graph) $
      cfgEntropy reducible_graph

testGraphMutilation :: CmmGraph -> Outcome
testGraphMutilation graph =
  compareWithEntropy (runcfg graph) (runcfg $ mutilate graph) $ cfgEntropy graph

runcfg :: CmmGraph -> BitConsumer Stmt Expr ()
runcfg = evalGraph stmt expr


type Entropy = [[Bool]]

----------------------------------------------------------------

mutilate :: CmmGraph -> CmmGraph
mutilate = unimp "mutilate"

compareWithEntropy :: BitConsumer Stmt Expr ()
                   -> BitConsumer Stmt Expr ()
                   -> Entropy
                   -> Outcome
compareWithEntropy = unimp "cwe"

cfgEntropy :: CmmGraph -> Entropy
cfgEntropy = unimp "entropy"

unimp :: String -> a
unimp s = error $ s ++ " not implemented"

----------------------------------------------------------------
runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'g'
  return (initUs_ us m)
