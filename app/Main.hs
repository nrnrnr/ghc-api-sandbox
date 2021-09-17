module Main where

import Control.Monad.IO.Class (liftIO)

import GHC
import GHC.Paths (libdir)
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )

import GHC.IO.Handle
import GHC.Utils.Outputable (printSDocLn, ppr, defaultUserStyle)
import GHC.Utils.Ppr (Mode(PageMode))

import System.Environment ( getArgs )
import System.IO (stdout)
import GHC.Plugins (defaultSDocContext)
import GHC.Stg.Syntax (StgTopBinding)
import GHC.CoreToStg (coreToStg)

main :: IO SuccessFlag
main = do
    args <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        -- flags <- mapM translate args
        --return $ mconcat flags
        cores <- mapM corify args
        return Succeeded

instance Semigroup SuccessFlag where
  (<>) Succeeded Succeeded = Succeeded
  (<>) _ _ = Failed

instance Monoid SuccessFlag where
  mempty = Succeeded

translate :: String -> GHC.Ghc SuccessFlag
translate pathname = do
  target <- guessTarget pathname Nothing Nothing
  setTargets [target]
  load LoadAllTargets

corify :: String -> GHC.Ghc CoreModule
corify pathname =
  do coremod <- compileToCoreSimplified pathname
     dflags <- getSessionDynFlags
     let doc = ppr (cm_binds coremod)
     liftIO $ printSDocLn defaultSDocContext (PageMode True) stdout doc
     return coremod

{-
stgify :: GHC.Ghc CoreModule -> [StgTopBinding]
stgify cm =
    do dflags <- getSessionDynFlags
       return $ bindings $ coreToStg dflags (cm_module cm) location program
    where bindings (bs, _, _) = bs

m :: ModLocation
m = undefined
-}

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
