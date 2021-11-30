{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where


import Control.Monad (when)

import DotCfg

import GHC
import GHC.CoreToStg
import GHC.CoreToStg.Prep
import GHC.Data.Stream hiding (mapM)
import GHC.Driver.Main ( hscParse, hscTypecheckRename, hscDesugar
                       , hscSimplify )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut
                          , initSDocContext
                          )

import GHC.Utils.Outputable ( printSDocLn, ppr, defaultUserStyle
                            , SDocContext
                            , Outputable
                            , OutputableP
                            , pdoc
--                            , SDoc
--                            , text
                            )
import GHC.Utils.Ppr (Mode(PageMode))
import GHC.Utils.Misc (fstOf3)
import GHC.Plugins (isDataTyCon)
import GHC.Types.HpcInfo (emptyHpcInfo)

import GHC.Cmm.Dataflow.Graph(NonLocal)
import GHC.Cmm.Node
import GHC.CmmToAsm.CFG()

import System.Environment ( getArgs )
import System.IO (stdout)
import GHC.Stg.Syntax (StgTopBinding, pprGenStgTopBindings, initStgPprOpts)
import GHC.Stg.FVs
import GHC.Unit.Module.ModGuts ( ModGuts(..) )

import StgToCmmLite (codeGen)
import GHC.Types.CostCentre (emptyCollectedCCs)
import GHC.Types.IPE (emptyInfoTableProvMap)
import GHC.Cmm (CmmGroup, GenCmmDecl(..), GenCmmGraph(..), Section(..))
import GHC.Platform (Platform)
--import GHC (GhcMonad(getSession))

libdir :: String
libdir = "/home/nr/asterius/ghc/_build/stage1/lib"

main :: IO ()
main = showStdCmm

showStdCmm :: IO ()
showStdCmm = do
    --putStrLn $ "libdir == " ++ thelibdir
    args <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just thelibdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        let sdctx = initSDocContext dflags defaultUserStyle
        targets <- mapM (\path -> guessTarget path Nothing Nothing) args
        setTargets targets
        mgraph <- depanal [] False
        mapM_ (\s -> --dumpSummary sdctx s >>
                     --liftIO (putStrLn "/* .............. */") >>
                     --dumpStg sdctx s >>
                     --liftIO (putStrLn "/* ------------------- */") >>
                     dumpCmm sdctx s) $ mgModSummaries mgraph
  where thelibdir = libdir

----------------------------------------------------------------

frontend :: DynFlags -> HscEnv -> ModSummary -> IO ModGuts
frontend _dflags env summary = do
   parsed <- hscParse env summary
   (checked, _) <- hscTypecheckRename env summary parsed
   hscDesugar env summary checked >>= hscSimplify env []

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

dumpStg :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpStg context summ = do
  dflags <- getSessionDynFlags
  env <- getSession
  guts <- liftIO $ frontend dflags env summ
  stg <- stgify summ guts
  liftIO $ printSDocLn context (PageMode True) stdout $
         pprGenStgTopBindings (initStgPprOpts dflags) stg

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
        decl _platform (CmmProc _h entry _registers graph) = do
          printSDocLn context (PageMode True) stdout $ dotCFG (ppr entry) graph



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

collectAll :: Monad m => Stream m a b -> m ([a], b)
collectAll = gobble . runStream
    where gobble (Done b) = return ([], b)
          gobble (Effect e) = e >>= gobble
          gobble (Yield a s) = do (as, b) <- gobble s
                                  return (a:as, b)


dumpSummary :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpSummary context summ =
  liftIO $ printSDocLn context (PageMode True) stdout $ ppr summ

