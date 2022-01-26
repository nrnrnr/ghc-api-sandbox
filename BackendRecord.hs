{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | What constitutes a back end for code generation 

module {-GHC.Driver.-}BackendRecord
   ( Backend'(..)
   )

where

import Data.Set(Set)
import GHC.Cmm
import GHC.Data.Stream
import GHC.Driver.Env.Types
import GHC.Driver.Pipeline.Phases
import GHC.Platform
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Session
import GHC.Unit.Module

import GHC.Driver.Phases


data Backend' dflags =
    Backend { 
            -- | An informal description of the back end, for use in
            -- issuing warning messages *only*.  If code depends on
            -- what's in the string, you deserve what happens to you.
              backendDescription :: !String


            -- | This flag tells the compiler driver whether the back
            -- end will write files: interface files and object files.
            -- It is typically true for "real" back ends that generate
            -- code into the filesystem.  (That means, not the interpreter.)
            , backendWritesFiles :: !Bool

            -- | When the back end does write files, this value tells
            -- the compiler in what manner of file the output should go:
            -- temporary, persistent, or specific.
            , backendPipelineOutput :: PipelineOutput

            , backendGeneratesCode :: !Bool
                 -- ^ if not, need to turn on code gen for TH.  If so,
                 -- supports HscRecomp.  If not, there is no output
                 -- file.  If not, recompilation does not need a
                 -- linkable (and is automatically up to date).
                                      -- And a ton of stuff in the driver.

            , backendSupportsInterfaceWriting :: !Bool
                 -- ^ Turn on interface writing for Backpack Should
                 -- probably be the same as `backendGeneratesCode`,
                 -- but is kept distinct for reasons described in Note
                 -- [-fno-code mode]

            -- | When preparing code for this back end, the type
            -- checker should pay attention to SPECIALISE pragmas.  If
            -- this flag is False, then the type checker ignores
            -- SPECIALISE pragmas. for imported things.
            , backendRespectsSpecialise :: !Bool


            -- | This back end wants the `mi_globals` field of a
            -- `ModIface` to be populated (with the top-level bindings
            -- of the original source).  True for the interpreter, and
            -- also true for "no backend", which is used by Haddock.
            -- After typechecking a module, Haddock wants access to
            -- the module's GlobalRdrEnv.
            , backendWantsGlobalBindings :: !Bool



            -- | The back end targets a technology that implements
            -- `switch` natively (e.g., LLVM or C).  Therefore it is
            -- not necessary for GHC to ccompile a Cmm Switch form
            -- into a decision tree with jump tables at the leaves.
            , backendHasNativeSwitch :: !Bool

            -- | The next two flags control the implementations of
            -- some primitive operations in GHC.StgToCmm.Prim.
            , backendWantsNcgPrimitives :: !Bool
            , backendWantsLlvmPrimitives :: !Bool


            -- | When `IsValid`, the back end is compatible with vector instructions.
            -- When `NotValid`, carries a message that is shown to users.
            , backendSimdValidity :: Validity' String

            -- | Can the back end support large binary blobs?  See
            -- Note [Embedding large binary blobs] in GHC.CmmToAsm.Ppr.
            , backendSupportsEmbeddedBlobs :: !Bool


            -- | This flag tells the compiler driver that the back end
            -- does not support every target platform; it supports
            -- only platforms that claim NCG support.  (It's set only
            -- for the native code generator.)  Crufty.  If the driver
            -- tries to use the native code generator *without*
            -- platform support, the driver fails over to the LLVM
            -- back end.
            , backendNeedsPlatformNcgSupport :: !Bool


            -- | This flag is set if the back end can generate code
            -- for proc points.  If the flag is not set, then a Cmm
            -- pass needs to split proc points (that is, turn each
            -- proc point into a standalone procedure).
            , backendSupportsUnsplitProcPoints :: !Bool


            -- | This flag guides the driver in resolving issues about
            -- API support on the target platform. If the flag is set,
            -- then these things are true:
            --
            --    * When the target platform supports *only* an unregisterised API,
            --      this backend can be replaced with compilation via C.  
            --
            --    * When the target does *not* support an unregisterised API,
            --      this back end can replace compilation via C.
            --
            , backendSwappableWithViaC :: !Bool


            -- | This flag is true if the back end works *only* with
            -- the unregisterised ABI.
            , backendUnregisterisedAbiOnly :: !Bool

            -- | This flag is set if the back end generates C code in
            -- a .hc file.  The flag is used to let the compiler
            -- driver know if the command-line flag `-C` is
            -- meaningful.
            , backendGeneratesHc :: !Bool



            -- The next four flags are True only for the interpreter.

            -- | SPT entries will be inserted dynamically if needed.
            -- If this flag is false, then GHC.Iface.Tidy should emit
            -- C stubs that initialize the SPT entries.
            , backendSptIsDynamic :: !Bool

            -- | If this flag is set, then GHC.HsToCore.Coverage
            -- inserts Breakpoint ticks.  Used only for the
            -- interpreter.
            , backendWantsBreakpointTicks :: !Bool

            -- | If this flag is set, then the driver forces the
            -- optimization level to 0, issuing a warning message if
            -- the command line requested a higher optimization level.
            , backendForcesOptimization0 :: !Bool

            -- | I don't understand exactly how this works.  But if
            -- this flag is set *and* another condition is met, then
            -- ghc/Main.hs will alter the dflags so that all the
            -- `hostFullWays` are asked for.  It is set only for the interpreter.
            , backendNeedsFullWays :: !Bool

            -- | Says whether the back end supports HPC. If not, the
            -- compiler driver will ignore the `-fhpc` option (and
            -- will issue a warning message if it is used).
            , backendSupportsHpc :: !Bool



            , backendValidityOfCExportStatic :: !Validity
            , backendValidityOfCImport :: !Validity



            ----------------- supporting tooling
            -- | The assembler used on the code that is written by this back end.
            -- A program determined by a combination of back end,
            -- DynFlags, and Platform is run with the given `Option`s.
            , backendAssemblerProg :: Logger -> dflags -> Platform -> [Option] -> IO ()

            -- | Used to retrieve an enumeration value that
            -- characterizes the C/assembler part of a toolchain.
            -- Caches the info in a mutable variable that is part of
            -- the DynFlags.
            , backendAssemblerInfo :: Logger -> dflags -> IO CompilerInfo


{-
        let (as_prog, get_asm_info) | backendWantsClangTools (backend dflags)
                    , platformOS platform == OSDarwin
                    = (GHC.SysTools.runClang, pure Clang)
                    | otherwise
                    = (GHC.SysTools.runAs, getAssemblerInfo logger dflags)
-}




            -- | When using this back end, it may be necessary or
            -- advisable to pass some `-D` options to a C compiler.
            -- This function produces those options, if any.  An IO
            -- action may be necessary in order to interrogate
            -- external tools about what version they are, for
            -- example.
            , backendCDefs :: Logger -> dflags -> IO [String]


            ----------------- code generation and compiler driver

            , backendCodeOutput :: forall a .
                                   Logger
                                -> dflags
                                -> Module -- this_mode
                                -> ModLocation
                                -> FilePath -- filenm
                                -> Set UnitId -- ^ Dependencies
                                -> Stream IO RawCmmGroup a -- linted_cmm_stream
                                -> IO a


            , backendPostHscPipeline :: forall m .
                                        TPipelineClass TPhase m
                                     => PipeEnv
                                     -> HscEnv
                                     -> Maybe ModLocation
                                     -> FilePath
                                     -> m (Maybe FilePath)

            , backendNormalSuccessorPhase :: Phase




            }
