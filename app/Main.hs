module Main where

import Control.Monad.IO.Class (liftIO)

import GHC
import GHC.CoreToStg
import GHC.CoreToStg.Prep
import GHC.Data.Stream hiding (mapM)
import GHC.Driver.Main ( hscParse, hscTypecheckRename, hscDesugar
                       , newHscEnv, hscSimplify )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )

import GHC.IO.Handle
import GHC.Utils.Outputable ( printSDocLn, ppr, defaultUserStyle
                            , SDocContext, 
                            )
import GHC.Utils.Ppr (Mode(PageMode))
import GHC.Utils.Misc (fstOf3)

import System.Environment ( getArgs )
import System.IO (stdout)
import GHC.Stg.Syntax (StgTopBinding, pprGenStgTopBindings, initStgPprOpts)
import GHC.CoreToStg (coreToStg)
import GHC.Driver.Session (initSDocContext)
import GHC.Plugins (isDataTyCon, fstOf3)
import GHC.Unit.Module.ModGuts ( ModGuts(..) )

--import StgReifyStack


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
  where thelibdir = libdir


----------------------------------------------------------------

frontend :: DynFlags -> ModSummary -> IO ModGuts
frontend dflags summary = do
   env <- newHscEnv dflags
   parsed <- hscParse env summary
   (checked, _) <- hscTypecheckRename env summary parsed
   hscDesugar env summary checked >>= hscSimplify env []

stgify :: ModSummary -> ModGuts -> Ghc [StgTopBinding]
stgify summary guts = do
    dflags <- getSessionDynFlags
    env <- liftIO $ newHscEnv dflags
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
  guts <- liftIO $ frontend dflags summ
  stg <- stgify summ guts
  liftIO $ printSDocLn context (PageMode True) stdout $
         pprGenStgTopBindings (initStgPprOpts dflags) stg

dumpCmm :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpCmm context summ = do
  dflags <- getSessionDynFlags
  guts <- liftIO $ frontend dflags summ
  stg <- stgify summ guts
  asmout <- undefined -- stream ThWarningAndDeprecationPragmas
  return ()

collectAll :: Monad m => Stream m a b -> m ([a], b)
collectAll = gobble . runStream
    where gobble (Done b) = return ([], b)
          gobble (Effect e) = e >>= gobble
          gobble (Yield a s) = do (as, b) <- gobble s
                                  return (a:as, b)


dumpSummary :: SDocContext -> ModSummary -> GHC.Ghc ()
dumpSummary context summ =
  liftIO $ printSDocLn context (PageMode True) stdout $ ppr summ


{-
hscDumpCmm :: HscEnv -> CgGuts -> ModLocation -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], CgInfos)
                -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts location output_filename = do
        let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                    -- From now on, we just use the bits we need.
                    cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_ccs      = local_ccs,
                    cg_tycons   = tycons,
                    cg_foreign  = foreign_stubs0,
                    cg_foreign_files = foreign_files,
                    cg_dep_pkgs = dependencies,
                    cg_hpc_info = hpc_info } = cgguts
            dflags = hsc_dflags hsc_env
            logger = hsc_logger hsc_env
            hooks  = hsc_hooks hsc_env
            tmpfs  = hsc_tmpfs hsc_env
            profile = targetProfile dflags
            data_tycons = filter isDataTyCon tycons
            -- cg_tycons includes newtypes, for the benefit of External Core,
            -- but we don't generate any code for newtypes

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        (prepd_binds) <- {-# SCC "CorePrep" #-}
                       corePrepPgm hsc_env this_mod location
                                   core_binds data_tycons

        -----------------  Convert to STG ------------------
        (stg_binds, denv, (caf_ccs, caf_cc_stacks))
            <- {-# SCC "CoreToStg" #-}
               withTiming logger
                   (text "CoreToStg"<+>brackets (ppr this_mod))
                   (\(a, b, (c,d)) -> a `seqList` b `seq` c `seqList` d `seqList` ())
                   (myCoreToStg logger dflags (hsc_IC hsc_env) False this_mod location prepd_binds)

        let cost_centre_info =
              (local_ccs ++ caf_ccs, caf_cc_stacks)
            platform = targetPlatform dflags
            prof_init
              | sccProfilingEnabled dflags = profilingInitCode platform this_mod cost_centre_info
              | otherwise = mempty

        ------------------  Code generation ------------------
        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so showPass isn't very useful here.
        -- Hence we have one showPass for the whole backend, the
        -- next showPass after this will be "Assembler".
        withTiming logger
                   (text "CodeGen"<+>brackets (ppr this_mod))
                   (const ()) $ do
            cmms <- {-# SCC "StgToCmm" #-}
                            doCodeGen hsc_env this_mod denv data_tycons
                                cost_centre_info
                                stg_binds hpc_info

            ------------------  Code output -----------------------
            rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                        case cmmToRawCmmHook hooks of
                            Nothing -> cmmToRawCmm logger profile cmms
                            Just h  -> h dflags (Just this_mod) cmms

            let dump a = do
                  unless (null a) $
                    putDumpFileMaybe logger Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
                  return a
                rawcmms1 = Stream.mapM dump rawcmms0

            let foreign_stubs st = foreign_stubs0 `appendStubC` prof_init
                                                  `appendStubC` cgIPEStub st

            (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, cg_infos)
                <- {-# SCC "codeOutput" #-}
                  codeOutput logger tmpfs dflags (hsc_units hsc_env) this_mod output_filename location
                  foreign_stubs foreign_files dependencies rawcmms1
            return (output_filename, stub_c_exists, foreign_fps, cg_infos)


-}
