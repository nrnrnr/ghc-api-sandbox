{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (liftIO)

import GHC
import GHC.CoreToStg
import GHC.CoreToStg.Prep
import GHC.Data.Stream hiding (mapM)
import GHC.Driver.Main ( hscParse, hscTypecheckRename, hscDesugar
                       , hscSimplify )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )

import GHC.IO.Handle
import GHC.Utils.Outputable ( printSDocLn, ppr, defaultUserStyle
                            , SDocContext
                            , Outputable
                            , OutputableP
                            , pdoc
                            )
import GHC.Utils.Ppr (Mode(PageMode))
import GHC.Utils.Misc (fstOf3)

import System.Environment ( getArgs )
import System.IO (stdout)
import GHC.Stg.Syntax (StgTopBinding, pprGenStgTopBindings, initStgPprOpts)
import GHC.Stg.FVs
import GHC.CoreToStg (coreToStg)
import GHC.Driver.Session (initSDocContext)
import GHC.Plugins (isDataTyCon, fstOf3)
import GHC.Unit.Module.ModGuts ( ModGuts(..) )

import StgToCmmLite (codeGen)
import GHC.Types.HpcInfo (HpcInfo(NoHpcInfo), emptyHpcInfo)
import GHC.Types.CostCentre (emptyCollectedCCs)
import GHC.Types.IPE (emptyInfoTableProvMap)
import GHC.Cmm (CmmGroup, GenCmmDecl(..), CmmGraph(..))
import GHC.Platform (genericPlatform, Platform (Platform))
import GHC (GhcMonad(getSession))


libdir = "/home/nr/asterius/ghc/_build/stage1/lib"

main :: IO ()
main = do
    putStrLn $ "libdir == " ++ thelibdir
    args <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just thelibdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        let sdctx = initSDocContext dflags defaultUserStyle
        targets <- mapM (\path -> guessTarget path Nothing Nothing) args
        setTargets $ targets
        mgraph <- depanal [] False
        mapM_ (\s -> dumpSummary sdctx s >>
                     liftIO (putStrLn "..............") >>
                     dumpStg sdctx s >>
                     liftIO (putStrLn "-------------------")) $ mgModSummaries mgraph
  where thelibdir = id libdir


----------------------------------------------------------------

frontend :: DynFlags -> HscEnv -> ModSummary -> IO ModGuts
frontend dflags env summary = do
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
  let stg' = annTopBindingsFreeVars stg
  let hpcinfo = emptyHpcInfo False
  (groups, (stub, infos)) <-
      liftIO $
      collectAll $
      codeGen logger dflags (ms_mod summ) infotable tycons ccs stg' hpcinfo
  liftIO $ mapM_ group groups
  where group :: CmmGroup -> IO ()
        group = mapM_ decl
        decl :: (OutputableP Platform d, OutputableP Platform h, OutputableP Platform g) =>
                GenCmmDecl d h g -> IO ()
        decl (CmmData {}) = putStrLn "(data section, not dumped)"
        decl (CmmProc h label registers graph) = do
          pprout context $ pdoc genericPlatform h
          pprout context label
          putStr "global registers" >> pprout context registers
          pprout context $ pdoc genericPlatform graph
          putStrLn "####################################"
                     

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

