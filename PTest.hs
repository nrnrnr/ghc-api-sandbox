{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import DotCfg

import Control.Monad
import Control.Monad.IO.Class

import PetersonR
import SCode

import System.FilePath as FilePath


import GHC
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Session
import GHC.Platform
import GHC.Unit.Home
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Ppr (Mode(PageMode))

import GHC.Cmm
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Parser

import System.Environment ( getArgs )
import System.IO (stdout)

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
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        let sdctx = initSDocContext dflags defaultUserStyle
        mapM_ (processCmm sdctx) args
  where thelibdir = libdir

processCmm :: SDocContext -> FilePath -> Ghc ()
processCmm context path = do
  env <- getSession
  dflags <- getSessionDynFlags
  group <- liftIO (slurpCmm env path)
  liftIO $ dumpGroup context (targetPlatform dflags) group

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
  where
        decl :: ( OutputableP Platform d
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
          printSDocLn context (PageMode True) stdout $ dotCFG (ppr entry) graph
          when True $ do
            putStrLn "/* ============="
            let code = structuredControl graph :: SCode
            pprout context $ unS code
            putStrLn "============== */"
            
          when True $ do
            putStrLn "/*********"
            pprout context $ pdoc platform h
            pprout context entry
            putStr "global registers" >> pprout context registers
            pprout context $ pdoc platform graph
            putStrLn "*********/"




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



dumpSummary :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpSummary context summ =
  liftIO $ printSDocLn context (PageMode True) stdout $ ppr summ

