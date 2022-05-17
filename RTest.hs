{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment ( getArgs )
import System.Exit

import GHC hiding (Stmt, Match)
import GHC.Cmm hiding (succ)
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dominators
import GHC.Cmm.Reducibility
import GHC.Driver.Session
import GHC.Platform
import GHC.Types.Unique.Supply

import qualified GHC.LanguageExtensions as LangExt

import ActionsAndObservations
import BitConsumer
import CmmPaths
import ControlTestMonad
import EntropyTransducer
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
        groups <- mapM loadPath files
        let platform = targetPlatform dflags
            labeled_groups = [(path, group)
                                  | (path, groups) <- zip files groups, group <- groups]
            run f = concat <$> mapM (concatMapGraphs platform (const f)) labeled_groups
        liftIO $ do
          reductions <- run testGraphReduction
          mutilations <- run (return . testGraphMutilation)
          results <- liftM2 (++) (mapM (analyze isIdentical) reductions)
                                 (mapM (analyze isDifferent) mutilations)
          exitWith $ foldl combineExits exitZero results


concatMapGraphs :: Monad m
             => Platform
             -> (Platform -> CmmGraph -> m a)
             -> (FilePath, CmmGroup)
             -> m [(FilePath, a)]
concatMapGraphs platform f (path, group) = concat <$> mapM (decl . cmmCfgOptsProc False) group
  where decl (CmmData {}) = return []
        decl (CmmProc _h _entry _registers graph) =
            do a <- f platform graph
               return [(path,a)]


count :: [a] -> String -> String
count xs thing = case length xs of
                   1 -> "1 " ++ thing
                   n -> show n ++ " " ++ thing ++ "s"


data Outcome = Identical { npaths :: Int }
             | Different { different :: [(Trace, Trace)], nsame :: Int }
type Trace = [Event Stmt Expr]

isDifferent, isIdentical :: Outcome -> Bool

isDifferent (Different {}) = True
isDifferent _ = False

isIdentical (Identical {}) = True
isIdentical _ = False

testGraphReduction :: CmmGraph -> IO Outcome
testGraphReduction original_graph = do
  reducible_graph <- fmap gwd_graph $ runUniqSM $
                     asReducible $ graphWithDominators original_graph
  return $ case reducibility (graphWithDominators original_graph) of
    Reducible -> id Identical 0
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
compareWithEntropy a b bit_streams =
   foldl add (Identical 0) $ map (compareRuns a b) bit_streams
  where add (Identical k) Match = Identical (succ k)
        add (Different ts k) Match = Different ts (succ k)
        add (Identical k) (NoMatch pair) = Different [pair] k
        add (Different ts k) (NoMatch pair) = Different (pair:ts) k

data SingleComparison = Match
                      | NoMatch (Trace, Trace)

compareRuns :: BitConsumer Stmt Expr ()
            -> BitConsumer Stmt Expr ()
            -> [Bool]
            -> SingleComparison
compareRuns a b bits =
    if and $ zipWith (==) aEvents bEvents then
        Match
    else
        NoMatch (aEvents, bEvents)
 where aEvents = pastEvents $ runWithBits a bits
       bEvents = pastEvents $ runWithBits b bits


cfgEntropy :: CmmGraph -> Entropy
cfgEntropy = map traceBits . cmmPaths

unimp :: String -> a
unimp s = error $ s ++ " not implemented"

----------------------------------------------------------------
runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'g'
  return (initUs_ us m)


combineExits :: ExitCode -> ExitCode -> ExitCode
exitZero :: ExitCode

exitZero = ExitSuccess
combineExits ExitSuccess e = e
combineExits e _ = e


analyze :: (Outcome -> Bool) -> (FilePath, Outcome) -> IO ExitCode
analyze isGood (path, outcome) = do
  putStrLn $ display $ path ++ ": " ++ case outcome of
    Identical n -> show n ++ " paths are identical"
    Different diffs nsame ->
       if nsame == 0 then
           "all " ++ show (length diffs) ++ " paths are different"
       else
           show (length diffs) ++ " of " ++ show (length diffs + nsame) ++ " paths are different"
  if isGood outcome then
      return ExitSuccess
  else
      return $ ExitFailure 1
 where display s = if isGood outcome then s ++ ", as expectec"
                   else "(FAULT!) " ++ s
