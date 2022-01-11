{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe
import Data.List hiding (foldl)

import DotCfg

import FlowTest
import GHC.Test.ControlMonad 

import Control.Monad
import Control.Monad.IO.Class

import GHC.Wasm.ControlFlow.OfCmm
import GHC.Wasm.ControlFlow

import GHC.Cmm.Dataflow.Dominators


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
import GHC.Driver.Session
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
import GHC.Cmm.Dataflow.Dominators.Lint
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Parser
import GHC.Cmm.Ppr()

import qualified GHC.LanguageExtensions as LangExt

import System.Environment ( getArgs )
import System.IO (stdout, stderr, hPutStrLn)

--import GHC.Wasm.ControlFlow
import GHC.Wasm.Ppr.Control()

libdir :: String
libdir = "/home/nr/asterius/ghc/_build/stage1/lib"

main :: IO ()
main = showGraph

showGraph :: IO ()
showGraph = do
    --putStrLn $ "libdir == " ++ thelibdir
    args <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just thelibdir) $ do
        raw_dflags <- getSessionDynFlags
        let dflags = raw_dflags `xopt_set` LangExt.MagicHash
                         `xopt_set` LangExt.StandaloneKindSignatures
                         `xopt_set` LangExt.UnliftedDatatypes
                         `xopt_set` LangExt.DataKinds
        setSessionDynFlags dflags
        let sdctx = initSDocContext dflags defaultUserStyle
        mapM_ (processPath sdctx) args
  where thelibdir = libdir

processPath :: SDocContext -> FilePath -> Ghc ()
processPath context path = do
    liftIO $ putStrLn $ "/*** INPUT: " ++ path ++ " */"
    case takeExtension path of
      ".hs" -> processHs context path
      ".cmm" -> processCmm context path
      _ -> liftIO $ hPutStrLn stderr $ "File with unknown extension: " ++ path
    liftIO $ putStrLn $ "/*** END: " ++ path ++ " */"
    liftIO $ putStrLn ""

processHs :: SDocContext -> FilePath -> Ghc ()
processHs context path = do
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  mgraph <- depanal [] False
  mapM_ (dumpSummary context) $ mgModSummaries mgraph

dumpSummary :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpSummary context summ = do
  dflags <- getSessionDynFlags
  env <- getSession
  guts <- liftIO $ frontend dflags env summ
  stg <- stgify summ guts
  logger <- getLogger
  let infotable = emptyInfoTableProvMap
  let tycons = []
  let ccs = emptyCollectedCCs
  let stg' = depSortWithAnnotStgPgm (ms_mod summ) stg
  let hpcinfo = emptyHpcInfo False
  (groups, (_stub, _infos)) <-
      liftIO $
      collectAll $
      codeGen logger dflags (ms_mod summ) infotable tycons ccs stg' hpcinfo
  liftIO $ mapM_ (dumpGroup context (targetPlatform dflags)) groups

frontend :: DynFlags -> HscEnv -> ModSummary -> IO ModGuts
frontend _dflags env summary = do
   parsed <- hscParse env summary
   (checked, _) <- hscTypecheckRename env summary parsed
   hscDesugar env summary checked >>= hscSimplify env []


processCmm :: SDocContext -> FilePath -> Ghc ()
processCmm context path = do
  env <- getSession
  dflags <- getSessionDynFlags
  group <- liftIO (slurpCmm env path)
  liftIO $ dumpGroup context (targetPlatform dflags) group

stgify :: ModSummary -> ModGuts -> Ghc [StgTopBinding]
stgify summary guts = do
    dflags <- getSessionDynFlags
    env <- getSession
    prepd_binds <- liftIO $ corePrepPgm env this_mod location core_binds data_tycons
    return $ fstOf3 $ coreToStg dflags (ms_mod summary) (ms_location summary) prepd_binds
  where this_mod = mg_module guts
        location = ms_location summary
        core_binds = mg_binds guts
        data_tycons = filter isDataTyCon tycons
        tycons = mg_tcs guts



----------------------------------------------------------------

slurpCmm :: HscEnv -> FilePath -> IO (CmmGroup)
slurpCmm hsc_env filename = runHsc hsc_env $ do
    let dflags   = hsc_dflags hsc_env
    let logger   = hsc_logger hsc_env
    let home_unit = hsc_home_unit hsc_env
        -- Make up a module name to give the NCG. We can't pass bottom here
        -- lest we reproduce #11784.
        mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
        cmm_mod = mkHomeModule home_unit mod_name
    (cmm, _) <- ioMsgMaybe
               $ do
                  (warns,errs,cmm) <- withTiming logger (text "ParseCmm"<+>brackets (text filename)) (\_ -> ())
                                       $ parseCmmFile dflags cmm_mod home_unit filename
                  let msgs = warns `unionMessages` errs
                  return (GhcPsMessage <$> msgs, cmm)
    return cmm


dumpGroup :: SDocContext -> Platform -> CmmGroup -> IO ()
dumpGroup context platform = mapM_ (decl platform . cmmCfgOptsProc False)
  where decl :: ( OutputableP Platform d
                , OutputableP Platform h
                , OutputableP Platform (GenCmmGraph node)
                , NonLocal node
                , node ~ CmmNode
                )
                => Platform
                -> GenCmmDecl d h (GenCmmGraph node)
                -> IO ()
        decl platform (CmmData (Section sty label) d) = when False $ do
          putStrLn $ show label ++ "(" ++ show sty ++ "):"
          pprout context $ pdoc platform d
        decl platform (CmmProc h entry registers graph) = do
          let r = reducibility (graphWithDominators graph)
          printSDocLn context (PageMode True) stdout $ dotCFG blockTag (ppr entry) graph

          when True $ do
            putStrLn "/*********"
            pprout context $ pdoc platform h
            pprout context entry
            putStr "global registers" >> pprout context registers
            pprout context $ pdoc platform graph
            putStrLn "*********/"

          when (True && r == Reducible) $ do
            putStrLn "/* ============= "
            let pprCode block = text "CODE:" <+> (fromMaybe (text "?") $ blockTagOO block)
                code = structuredControl platform (\_ -> id) (\_ -> pprCode) graph
            pprout context $ pdoc platform code
            putStrLn "============== */"

          when True $ do
            putStrLn "/* $$$$$$$$$$$$$ "
            putStrLn $ "  Dominators " ++
                         (if dominatorsPassAllChecks graph then "pass" else "FAIL") ++
                         " lint checks"
            mapM_ (putStrLn . showSDocUnsafe . ppr) (dominatorsFailures graph)
            putStrLn "   PATHS:"
            let --pprLabel = blockTag . blockLabeled graph 
                pprLabel = ppr
                pprPath' lbls = hcat $ intersperse (text " -> ") (map pprLabel (reverse lbls))
            pprout context $ vcat (map pprPath' $ shortPaths' graph)
            putStrLn "$$$$$$$$$$$$$$ */"

          when False $ do 
            let (results, ios) = unzip $ map analyzeTest $ cmmPathResults graph
            putStrLn "/* <<<<<<<<<<<<<<<<< "
            putStrLn $ "Testing CMM " ++ show (length $ cmmPathResults graph) ++ " path results"
            putStrLn $ "CMM:  " ++ resultReport results
            sequence_ ios
            putStrLn "   >>>>>>>>>>>>>>>>> */ "

          when (True && r == Reducible) $ do 
            let (results, ios) = unzip $ map analyzeTest $ wasmPathResults platform graph
            putStrLn "/* ||||||||||||||||||| "
            putStrLn $ "Testing Wasm " ++ show (length $ wasmPathResults platform graph) ++ " path results"
            putStrLn $ "Wasm: " ++ resultReport results
            sequence_ ios
            putStrLn "   |||||||||||||||||| */ "

          when (True && r == Reducible) $ do
            putStrLn "/* Peephole: @@@@@@@@@@@@@@@@@@@@ "
            let pprCode block = text "CODE:" <+> (fromMaybe (text "?") $ blockTagOO block)
                code = wasmPeepholeOpt $
                       structuredControl platform (\_ -> id) (\_ -> pprCode) graph
            pprout context $ pdoc platform code
            putStrLn "@@@@@@@@@@@@@@@@@@@ */"



          when (True && r == Reducible) $ do 
            let (results, ios) = unzip $ map analyzeTest $ wasmPeepholeResults platform graph
            putStrLn "/* ##################### "
            putStrLn $ "Testing peephole " ++ show (length $ wasmPeepholeResults platform graph) ++ " path results"
            putStrLn $ "Peep: " ++ resultReport results
            sequence_ ios
            putStrLn "   ##################### */ "

        resultReport results =
            if good == total then "All " ++ show total ++ " results are good"
            else show good ++ " of " ++ show total ++ " results are good"
          where total = length results
                good = length [r | r <- results, r == Good]

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

  


blockLabeled :: NonLocal n => GenCmmGraph n -> Label -> Block n C C
blockLabeled g lbl =
    mapFindWithDefault (panic "bad label") lbl (graphMap g)

{-
dumpCmm :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpCmm context summ = do
  dflags <- getSessionDynFlags
  env <- getSession
  guts <- liftIO $ frontend dflags env summ
  stg <- stgify summ guts
  logger <- getLogger
  let infotable = emptyInfoTableProvMap
  let tycons = []
  let ccs = emptyCollectedCCs
  let stg' = depSortWithAnnotStgPgm (ms_mod summ) stg
  let hpcinfo = emptyHpcInfo False
  (groups, (_stub, _infos)) <-
      liftIO $
      collectAll $
      codeGen logger dflags (ms_mod summ) infotable tycons ccs stg' hpcinfo
  liftIO $ mapM_ (group (targetPlatform dflags)) groups
  where group :: Platform -> CmmGroup -> IO ()
        group platform = mapM_ (decl platform)
        decl :: ( OutputableP Platform d
                , OutputableP Platform h
                , OutputableP Platform (GenCmmGraph node)
                , NonLocal node
                )
                => Platform
                -> GenCmmDecl d h (GenCmmGraph node)
                -> IO ()
        decl platform (CmmData (Section sty label) d) = when False $ do
          putStrLn $ show label ++ "(" ++ show sty ++ "):"
          pprout context $ pdoc platform d
        decl platform (CmmProc h entry registers graph) = do
          printSDocLn context (PageMode True) stdout $ dotCFG (ppr entry) graph
          when True $ do
            putStrLn "/*********"
            pprout context $ pdoc platform h
            pprout context entry
            putStr "global registers" >> pprout context registers
            pprout context $ pdoc platform graph
            putStrLn "*********/"
                     
-}


pprout :: Outputable a => SDocContext -> a -> IO ()
pprout context = printSDocLn context (PageMode True) stdout . ppr
            

{- What is outputable?

CmmStatic

ListGraph a
GenBasicBlock a


What goes into a CmmGroup:

  - CmmGroup == GenCmmGroup CmmStatics CmmTopInfo CmmGraph

  - 

  - CmmGraph in OutputableP Platform

  - GenCmmDecl


-}




data Summary = Waiting | AssignedLabel CmmReg CLabel | Called CLabel


blockTagOO :: Block CmmNode O O -> Maybe SDoc
blockTagOO b =
    case filter notTick nodes of
      [CmmAssign _ (CmmMachOp _ [CmmReg (CmmGlobal (VanillaReg {}))])] -> Just $ text "prologue"
      [CmmAssign _ (CmmReg (CmmGlobal (VanillaReg {})))] -> Just $ text "prologue"
      -- [CmmAssign {}] -> Just $ text "single :="
      _ -> case foldl extend Waiting (blockToList b) of
               Waiting -> Nothing
               Called l -> Just $ pdoc genericPlatform l
               AssignedLabel r l -> Just $ ppr r <+> text ":=" <+> ppr l
  where extend :: Summary -> CmmNode O O -> Summary
        extend Waiting (CmmAssign r (CmmLit (CmmLabel l))) = AssignedLabel r l
        extend (AssignedLabel r l) (CmmUnsafeForeignCall (ForeignTarget e _) _ _)
           | CmmLit (CmmLabel l') <- e = Called l'
           | CmmReg r' <- e,  r == r' = Called l
        extend summary _ = summary                          
        notTick (CmmTick _) = False
        notTick _ = True

        nodes = blockToList b

blockTag :: Block CmmNode C C -> SDoc
blockTag b =
  fromMaybe (hcat [ppr (entryLabel b), text ": ..."]) (blockTagOO $ blockBody b)

blockBody :: Block CmmNode C C -> Block CmmNode O O
blockBody (BlockCC _first middle _last) = middle


collectAll :: Monad m => Stream m a b -> m ([a], b)
collectAll = gobble . runStream
    where gobble (Done b) = return ([], b)
          gobble (Effect e) = e >>= gobble
          gobble (Yield a s) = do (as, b) <- gobble s
                                  return (a:as, b)

