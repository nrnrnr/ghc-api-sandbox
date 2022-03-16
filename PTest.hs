{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Prelude

import Words

import Crypto.Hash.SHA1

import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Maybe
import Data.List hiding (foldl)

import DotCfg
import DotGraph

import GHC.Cmm.Collapse
import GHC.Cmm.Reducibility
import GHC.Data.Graph.Collapse

import FlowTest
--import GHC.Test.ControlMonad
import GHC.Types.Unique.Supply

import Control.Monad
import Control.Monad.IO.Class

import Attic.UnoptControlTx
import qualified GHC.Wasm.ControlFlow.FromCmm as Opt

import GHC.Driver.Config.StgToCmm (initStgToCmmConfig)

import GHC.Wasm.ControlFlow

import GHC.Cmm.Dominators

--import Debug.Trace

import Options.Applicative hiding (empty)


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
import GHC.Utils.Outputable hiding ((<>))
import qualified GHC.Utils.Outputable as Outputable
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

--import System.Environment ( getArgs )
import System.IO (stdout, stderr, hPutStrLn, hFlush)

import TxTest

--import GHC.Wasm.ControlFlow
import GHC.Wasm.Ppr.Control()

libdir :: String
libdir = "/home/nr/.ghc-snapshots/current/stage1/lib"

main :: IO ()
main = showGraph

showGraph :: IO ()
showGraph = do
    --putStrLn $ "libdir == " ++ thelibdir
    controls <- execParser opts
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just thelibdir) $ do
        raw_dflags <- getSessionDynFlags
        let dflags = raw_dflags `xopt_set` LangExt.MagicHash
                         `xopt_set` LangExt.StandaloneKindSignatures
                         `xopt_set` LangExt.UnliftedDatatypes
                         `xopt_set` LangExt.DataKinds
        setSessionDynFlags dflags
        let sdctx = initSDocContext dflags defaultUserStyle
        mapM_ (processPath sdctx controls) (files controls)
  where thelibdir = libdir

processPath :: SDocContext -> Controls -> FilePath -> Ghc ()
processPath context controls path = do
    liftIO $ putStrLn $ "/*** INPUT: " ++ path ++ " */"
    case takeExtension path of
      ".hs" -> processHs context controls path
      ".cmm" -> processCmm context controls path
      _ -> liftIO $ hPutStrLn stderr $ "File with unknown extension: " ++ path
    liftIO $ putStrLn $ "/*** END: " ++ path ++ " */"
    liftIO $ putStrLn ""

processHs :: SDocContext -> Controls -> FilePath -> Ghc ()
processHs context controls path = do
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  mgraph <- depanal [] False
  mapM_ (dumpSummary context controls) $ mgModSummaries mgraph

dumpSummary :: SDocContext -> Controls -> ModSummary -> GHC.Ghc ()
dumpSummary context controls summ = do
  dflags <- getSessionDynFlags
  env <- getSession
  guts <- liftIO $ frontend dflags env summ
  stg <- stgify summ guts
  logger <- getLogger
  let infotable = emptyInfoTableProvMap
      tycons = []
      ccs = emptyCollectedCCs
      stg' = depSortWithAnnotStgPgm (ms_mod summ) stg
      hpcinfo = emptyHpcInfo False
      tmpfs = hsc_tmpfs env
      stg_to_cmm dflags mod = codeGen logger tmpfs (initStgToCmmConfig dflags mod)
  (groups, _infos) <-
      liftIO $
      collectAll $
      stg_to_cmm dflags (ms_mod summ) infotable tycons ccs stg' hpcinfo
  liftIO $ mapM_ (dumpGroup context controls (targetPlatform dflags)) groups

frontend :: DynFlags -> HscEnv -> ModSummary -> IO ModGuts
frontend _dflags env summary = do
   parsed <- hscParse env summary
   (checked, _) <- hscTypecheckRename env summary parsed
   hscDesugar env summary checked >>= hscSimplify env []


processCmm :: SDocContext -> Controls -> FilePath -> Ghc ()
processCmm context controls path = do
  env <- getSession
  dflags <- getSessionDynFlags
  group <- liftIO (slurpCmm env path)
  liftIO $ dumpGroup context controls (targetPlatform dflags) group

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
----------------------------------------------------------------

-- control over display


data Controls = Controls
  { lang_cmm :: Bool
  , lang_wasm :: Bool
  , lang_unopt :: Bool
  , lang_peephole :: Bool

  , viz_dot :: Bool
  , viz_code :: Bool
  , viz_path :: Bool

  , path_results :: Bool

  , dominator_check :: Bool

  , node_split :: Bool

  , files :: [String]
  }

options :: Parser Controls
options = Controls
  <$> switch ( long "cmm" <> help "visualize/run Cmm" )
  <*> switch ( long "wasm" <> help "visualize/run optimized Wasm" )
  <*> switch ( long "unopt" <> help "visualize/run unoptimized Wasm" )
  <*> switch ( long "peephole" <> help "visualize/run peephole-optimized Wasm" )

  <*> switch ( long "dot" <> help "show dot visualization" )
  <*> switch ( long "code" <> help "print code" )
  <*> switch ( long "path" <> help "show paths" )

  <*> switch ( long "results" <> help "show results from running paths" )

  <*> switch ( long "dom" <> long "dominators" <> help "check dominators" )

  <*> switch ( long "split" <> help "show node-splitting" )

  <*> many (argument str (metavar "*.{hs,cmm}"))

opts :: ParserInfo Controls
opts = info (options <**> helper)
          (  fullDesc
          <> progDesc "Run experiments on .hs and .cmm code"
          <> header "ptest - test bed for compiler stuff"
          )


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


dumpGroup :: SDocContext -> Controls -> Platform -> CmmGroup -> IO ()
dumpGroup context controls platform = mapM_ (decl platform . cmmCfgOptsProc False)
  where decl :: ( OutputableP Platform d
                , OutputableP Platform h
                , OutputableP Platform (GenCmmGraph node)
                , NonLocal node
                , node ~ CmmNode
                )
                => Platform
                -> GenCmmDecl d h (GenCmmGraph node)
                -> IO ()
        printdoc = printSDocLn context (PageMode True) stdout
        decl platform (CmmData (Section sty label) d) = when False $ do
          putStrLn $ show label ++ "(" ++ show sty ++ "):"
          pprout context $ pdoc platform d
        decl platform (CmmProc h entry registers og_graph) = do
          let r = reducibility (graphWithDominators og_graph)
          gwd <- runUniqSM $ asReducible (graphWithDominators og_graph)
          let r_graph = gwd_graph gwd -- reducible graph
              mkResults tx = wasmResults r_graph (tx platform const const r_graph)
              wasmOptResults = mkResults Opt.structuredControl
              wasmUnoptResults = mkResults structuredControl
              wasmPeepholeResults =
                  mkResults (\p c1 c2 g -> wasmPeepholeOpt $ structuredControl p c1 c2 g)


          when (viz_dot controls && lang_cmm controls) $ do
            putStrLn $ "/** ORIGINAL " ++ show r ++ " **/"
            printdoc $ dotCFG blockTag (ppr entry) og_graph

          when (viz_code controls && lang_cmm controls) $ do
            putStrLn "/*********"
            pprout context $ pdoc platform h
            pprout context entry
            putStr "global registers" >> pprout context registers
            pprout context $ pdoc platform og_graph
            putStrLn "*********/"
            hFlush stdout

          when (node_split controls) $ do
            when (viz_dot controls) $ do
              putStrLn "/* Original graph: */"
              printdoc $
                dotCFG (hashTag platform) (text "ORIGINAL:" <+> ppr entry) og_graph
              putStrLn "/* Converted to reducible graph: */"
              printdoc $
                dotCFG (hashTag platform) (text "AS REDUCIBLE:" <+> ppr entry) r_graph
              hFlush stdout
            when (viz_code controls) $
              case r of
                Irreducible -> do
                  putStrLn "/*********"
                  pprout context $ pdoc platform h
                  pprout context entry
                  putStr "global registers" >> pprout context registers
                  pprout context $ pdoc platform r_graph
                  putStrLn "*********/"
                Reducible -> putStrLn "/***** original graph was reducible ****/"

          when (viz_code controls && lang_unopt controls) $ do
            putStrLn "/* ============= Unoptimized wasm "
            let pprCode block = text "CODE:" <+> (fromMaybe (text "?") $ blockTagOO block)
                code = structuredControl platform (\_ -> id) (\_ -> pprCode) r_graph
            pprout context $ pdoc platform code
            putStrLn "============== end unoptimized */"
            hFlush stdout

          when (viz_code controls && lang_wasm controls) $ do
            putStrLn "/* ^^^^^^^^^^^^^ Optimized wasm "
            let pprCode block = text "CODE:" <+> (fromMaybe (text "?") $ blockTagOO block)
                code = Opt.structuredControl platform (\_ -> id) (\_ -> pprCode) r_graph
            pprout context $ pdoc platform code
            putStrLn "^^^^^^^^^^^^^^ End optimized */"
            hFlush stdout

          when (dominator_check controls) $ do
            putStrLn "/* $-$-$-$-$-$-$ "
            putStrLn $ "  Dominators " ++
                         (if dominatorsPassAllChecks r_graph then "pass" else "FAIL") ++
                         " lint checks"
            mapM_ (putStrLn . showSDocUnsafe . ppr) (dominatorsFailures r_graph)
            putStrLn " $-$-$-$-$-$-$ */"

          when (viz_path controls && lang_cmm controls) $ do
            putStrLn "/* $$$$$$$$$$$$$ "
            putStrLn "   PATHS:"
            let --pprLabel = blockTag . blockLabeled graph
                pprLabel = ppr
                pprPath' lbls = hcat $ intersperse (text " -> ") (map pprLabel (reverse lbls))
            pprout context $ vcat (map pprPath' $ shortPaths' r_graph)
            putStrLn "$$$$$$$$$$$$$$ */"
            hFlush stdout

          when (path_results controls && lang_cmm controls) $ do
            let (results, ios) = unzip $ map analyzeTest $ cmmPathResults r_graph
            putStrLn "/* <<<<<<<<<<<<<<<<< "
            putStrLn $ "Testing CMM " ++ show (length $ cmmPathResults r_graph) ++ " path results"
            putStrLn $ "CMM:  " ++ resultReport results
            sequence_ ios
            putStrLn "   >>>>>>>>>>>>>>>>> */ "
            hFlush stdout

          when (path_results controls && lang_unopt controls) $ do
            let (results, ios) = unzip $ map analyzeTest $ wasmUnoptResults
            putStrLn "/* ||||||||||||||||||| "
            putStrLn $ "Testing unoptimized Wasm " ++ show (length $ wasmUnoptResults) ++ " path results"
            putStrLn $ "Wasm: " ++ resultReport results
            sequence_ ios
            putStrLn "   |||||||||||||||||| */ "
            hFlush stdout

          when (viz_code controls && lang_peephole controls) $ do
            putStrLn "/* Peephole: @@@@@@@@@@@@@@@@@@@@ "
            let pprCode block = text "CODE:" <+> (fromMaybe (text "?") $ blockTagOO block)
                code = wasmPeepholeOpt $
                       structuredControl platform (\_ -> id) (\_ -> pprCode) r_graph
            pprout context $ pdoc platform code
            putStrLn "@@@@@@@@@@@@@@@@@@@ end peephole */"
            hFlush stdout

          when (path_results controls && lang_wasm controls) $ do
            let (results, ios) = unzip $ map analyzeTest $ wasmOptResults
            putStrLn "/* ##################### "
            putStrLn $ "Testing optimized wasm " ++ show (length wasmOptResults) ++ " path results"
            putStrLn $ "Optimized wasm: " ++ resultReport results
            sequence_ ios
            putStrLn "   ##################### */ "
            hFlush stdout

          when (path_results controls && lang_peephole controls) $ do
            let (results, ios) = unzip $ map analyzeTest $ wasmPeepholeResults
            putStrLn "/* ##################### "
            putStrLn $ "Testing peephole " ++ show (length wasmPeepholeResults) ++ " path results"
            putStrLn $ "Peep: " ++ resultReport results
            sequence_ ios
            putStrLn "   ##################### */ "
            hFlush stdout

          when (node_split controls) $ do
            let dump selected graph =
                    printSDocLn context (PageMode True) stdout $
                    dotGraph showInfo selected graph
                showEvent (Finish g) =
                    putStrLn "/* final graph: */" >> dump [] g
                showEvent (SplitAt g k) =
                    putStrLn "/* splitting node: */" >> dump (props k splitProps) g
                showEvent (ConsumeBy to _from g) =
                    putStrLn "/* absorbing node: */" >> dump (selected to) g
                props k ps = [(k, ps)]
                splitProps = [("peripheries", int 3), ("color", text "blue")]

                pprLabel = blockTag' empty . blockLabeled og_graph

                showInfo :: (Int, CollapseInfo) -> SDoc
                showInfo (_, info) = unsplit `commaCat` split
                 where commaCat :: SDoc -> SDoc -> SDoc
                       commaCat a b
                           | isEmpty defaultSDocContext a = b
                           | isEmpty defaultSDocContext b = a
                           | otherwise =  a `to` comma <+> b
                       split = render "split" $ splitLabels info
                       unsplit = pprLabels $ setElems $ unsplitLabels info
                       render tag labels =
                           if setNull labels then empty
                           else text tag `to` text ":" <+> pprLabels (setElems labels)
                       pprLabels = pprWithCommas pprLabel

            mapM_ showEvent $ collapseCmm og_graph



        resultReport results =
            if good == total then "All " ++ show total ++ " results are good"
            else show good ++ " of " ++ show total ++ " results are good"
          where total = length results
                good = length [r | r <- results, r == Good]






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
blockTag = blockTag' (text ": ...")

to :: SDoc -> SDoc -> SDoc
to = (Outputable.<>)

blockTagX :: Block CmmNode C C -> SDoc
blockTagX b = case blockTagOO $ blockBody b of
                Just doc -> doc `to` parens (ppr (entryLabel b))
                Nothing -> ppr (entryLabel b)

blockTag' :: SDoc -> Block CmmNode C C -> SDoc
blockTag' suffix b =
  fromMaybe (hcat [ppr (entryLabel b), suffix]) (blockTagOO $ blockBody b)

blockBody :: Block CmmNode C C -> Block CmmNode O O
blockBody (BlockCC _first middle _last) = middle


collectAll :: Monad m => Stream m a b -> m ([a], b)
collectAll = gobble . runStream
    where gobble (Done b) = return ([], b)
          gobble (Effect e) = e >>= gobble
          gobble (Yield a s) = do (as, b) <- gobble s
                                  return (a:as, b)


runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'g'
  return (initUs_ us m)


hashBlock :: Platform -> CmmBlock -> ByteString
hashBlock platform b = hash $ fromString s
  where (_, code, _) = blockSplit b
        s = showSDocUnsafe $ pdoc platform code

hashTag :: Platform -> Block CmmNode C C -> SDoc
hashTag platform = sep . map text . take 2 . natWords . bsNat . hashBlock platform
