module Main where

import GHC.Platform
import qualified GHC.Driver.Backend.Legacy as L
import qualified GHC.Driver.Backend as B
import GHC.Driver.Pipeline.Monad
import GHC.Utils.Error
import GHC.Utils.Outputable

value :: Show a => (backend -> a) -> (backend -> String)
value f = show . f

bool :: (a -> Bool) -> (a -> String)
bool f = show . f

smod :: (a -> Bool -> Maybe String) -> (a -> String)
smod f a = show [f a b | b <- [True, False]]

validity :: (a -> Validity) -> (a -> String)
validity = validity' showSDocUnsafe

validity' :: (b -> String) -> (a -> Validity' b) -> (a -> String)
validity' render v backend = case v backend of IsValid -> "valid"
                                               NotValid a -> "not valid: " ++ render a


platform :: (a -> String) -> (Platform -> a) -> (a -> String)
platform desc f _backend = show (map (desc . f) platforms)
  where platforms = [genericPlatform]


backends :: [(String, L.Backend, B.Backend)]
backends = [ ("NCG", L.ncgBackend, B.ncgBackend)
           , ("LLVM", L.llvmBackend, B.llvmBackend)
           , ("ViaC", L.viaCBackend, B.viaCBackend)
           , ("Interpreter", L.interpreterBackend, B.interpreterBackend)
           , ("NoBackend", L.noBackend, B.noBackend)
           ]



data Result = Result { backend :: String
                     , test :: String
                     , legacy :: String
                     , new :: String
                     }

putResult :: Result -> IO ()
putResult r =
  putStrLn $ concat $ ["On test ", test r, ", the legacy ", backend r, " back end returned ",
                       legacy r, ", but the new back end returned ", new r]

results :: [Result]
results =
    [ Result backend test (oldF oldB) (newF newB)
    | (backend, oldB, newB) <- backends
    , (test, oldF, newF) <- tests
    ]

failures :: [Result]
failures = [r | r <- results, legacy r /= new r]

tests :: [(String, L.Backend -> String, B.Backend -> String)]
tests =
   [ ("backendGeneratesCode", bool L.backendGeneratesCode, bool B.backendGeneratesCode)
   , ("platformDefaultBackend"
     , platform L.backendDescription L.platformDefaultBackend
     , platform B.backendDescription B.platformDefaultBackend
     )
--   , ("platformNcgSupported", bool L.platformNcgSupported, bool B.platformNcgSupported)
   , ("backendGeneratesCode", bool L.backendGeneratesCode, bool B.backendGeneratesCode)
   , ("backendWantsGlobalBindings", bool L.backendWantsGlobalBindings, bool B.backendWantsGlobalBindings)
--   , ("backendCDefs", L.backendCDefs, bool B.backendCDefs)
--   , ("backendWantsClangTools", bool L.backendWantsClangTools, bool B.backendWantsClangTools)
   , ("backendNeedsFullWays", bool L.backendNeedsFullWays, bool B.backendNeedsFullWays)
   , ("backendPrimitiveImplementation"
     , value L.backendPrimitiveImplementation
     , value B.backendPrimitiveImplementation
     )
   , ("backendHasNativeSwitch", bool L.backendHasNativeSwitch, bool B.backendHasNativeSwitch)
   , ("backendValidityOfCExport", validity L.backendValidityOfCExport, validity B.backendValidityOfCExport)
   , ("backendValidityOfCImport", validity L.backendValidityOfCImport, validity B.backendValidityOfCImport)
   , ("backendGeneratesHc", bool L.backendGeneratesHc, bool B.backendGeneratesHc)
   , ("backendSupportsHpc", bool L.backendSupportsHpc, bool B.backendSupportsHpc)
   , ("backendNeedsPlatformNcgSupport", bool L.backendNeedsPlatformNcgSupport, bool B.backendNeedsPlatformNcgSupport)
   , ("backendUnregisterisedAbiOnly", bool L.backendUnregisterisedAbiOnly, bool B.backendUnregisterisedAbiOnly)
   , ("backendSwappableWithViaC", bool L.backendSwappableWithViaC, bool B.backendSwappableWithViaC)
   , ("backendDescription", L.backendDescription, B.backendDescription)
   , ("backendForcesOptimization0", bool L.backendForcesOptimization0, bool B.backendForcesOptimization0)
   , ("backendSupportsUnsplitProcPoints", bool L.backendSupportsUnsplitProcPoints, bool B.backendSupportsUnsplitProcPoints)
   , ("backendSpecialModuleSource", smod (flip L.backendSpecialModuleSource), smod B.backendSpecialModuleSource)
   , ("backendWantsBreakpointTicks", bool L.backendWantsBreakpointTicks, bool B.backendWantsBreakpointTicks)
   , ("backendSupportsEmbeddedBlobs", bool L.backendSupportsEmbeddedBlobs, bool B.backendSupportsEmbeddedBlobs)
   , ("backendSimdValidity", validity' id L.backendSimdValidity, validity' id B.backendSimdValidity)
   , ("backendSptIsDynamic", bool L.backendSptIsDynamic, bool B.backendSptIsDynamic)
   , ("backendSupportsInterfaceWriting", bool L.backendSupportsInterfaceWriting, bool B.backendSupportsInterfaceWriting)
   , ("backendRespectsSpecialise", bool L.backendRespectsSpecialise, bool B.backendRespectsSpecialise)
   , ("backendWritesFiles", bool L.backendWritesFiles, bool B.backendWritesFiles)
   , ("backendNormalSuccessorPhase", value L.backendNormalSuccessorPhase, value B.backendNormalSuccessorPhase)
--   , ("backendPipelineOutput", bool L.backendPipelineOutput, bool B.backendPipelineOutput)
   ]

isValid :: Validity' a -> Bool
isValid IsValid = True
isValid (NotValid _) = False

isNoOutputFile :: PipelineOutput -> Bool
isNoOutputFile NoOutputFile = True
isNoOutputFile _ = False

main :: IO ()
main =  mapM_ putResult failures
