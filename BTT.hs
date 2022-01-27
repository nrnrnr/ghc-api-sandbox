module Main where

import Data.List (intercalate, sort)
import Data.Map (Map, insertWith, toList, empty)

import GHC.Driver.Backend
import GHC.Driver.Backend.Rep
import GHC.Driver.Pipeline.Monad
import GHC.Utils.Error

isValid :: Validity' a -> Bool
isValid IsValid = True
isValid (NotValid _) = False

isNoOutputFile :: PipelineOutput -> Bool
isNoOutputFile NoOutputFile = True
isNoOutputFile _ = False

main :: IO ()
main = do
  mapM_ putPair $ filter interesting $ toList info
  mapM_ putTT predicates
 where info = foldl addPredicate empty predicates
       interesting (key, btt) =
           case length (primal btt) `compare` length (complement btt) of
             GT -> True
             EQ -> snd (head key)
             LT -> False

       showKey key = "T: " ++ commas (map fst $ filter snd key) ++ "; " ++
                     "F: " ++ commas (map fst $ filter (not . snd) key)
       putPair (key, btt) = do
         putStrLn ""
         putStrLn $ showKey key
         putStrLn $ "  primal:     " ++ commas (primal btt)
         putStrLn $ "  complement: " ++ commas (complement btt)

       commas ss = intercalate ", " ss


       putTT (name, p) = do
           putStr name
           putStr ": "
           putStrLn $ commas $ reverse $ sort $ map (short . p . snd) backends
         where short True = "T"
               short False = "F"
                                     

data BTT = BTT { primal :: [String] -- functions with that table
               , complement :: [String] -- functions with the complement
               }
  deriving (Show)

backends :: [(String, Backend)]
backends = [ ("NCG", NCG)
           , ("LLVM", LLVM)
           , ("ViaC", ViaC)
           , ("Interpreter", Interpreter)
           , ("NoBackend", NoBackend)
           ]

bttUnion :: BTT -> BTT -> BTT
bttUnion b1 b2 = BTT { primal = primal b1 ++ primal b2
                     , complement = complement b1 ++ complement b2
                     }

addPredicate :: Map [(String, Bool)] BTT -> (String, Backend -> Bool) -> Map [(String, Bool)] BTT
addPredicate map (name, f) =
    let p = table f
        c = [(a, not b) | (a, b) <- p]
    in  insertWith bttUnion p (BTT [name] []) $
        insertWith bttUnion c (BTT [] [name]) $
        map


table :: (Backend -> Bool) -> [(String, Bool)]
table f = [(name, f b) | (name, b) <- backends]

predicates :: [(String, Backend -> Bool)]
predicates = 
   [ ("backendWritesFiles", backendWritesFiles)
   , ("backendGeneratesCode", backendGeneratesCode)
   , ("backendWantsGlobalBindings", backendWantsGlobalBindings)

   , ("backendWantsClangTools", backendWantsClangTools)

   , ("backendNeedsFullWays", backendNeedsFullWays)

   , ("ncgFlavor . backendPrimitiveImplementation", ncgFlavor . backendPrimitiveImplementation)
   , ("llvmFlavor . backendPrimitiveImplementation", llvmFlavor . backendPrimitiveImplementation)



   , ("backendHasNativeSwitch", backendHasNativeSwitch)

   , ("isValid . backendValidityOfCExport", isValid . backendValidityOfCExport)
   , ("isValid . backendValidityOfCImport", isValid . backendValidityOfCImport)
   , ("isNoOutputFile . backendPipelineOutput", isNoOutputFile . backendPipelineOutput)

   , ("backendGeneratesHc", backendGeneratesHc)

   , ("backendSupportsHpc", backendSupportsHpc)
   , ("backendNeedsPlatformNcgSupport", backendNeedsPlatformNcgSupport)

   , ("backendUnregisterisedAbiOnly", backendUnregisterisedAbiOnly)
   , ("backendSwappableWithViaC", backendSwappableWithViaC)

   , ("backendForcesOptimization0", backendForcesOptimization0)
   , ("backendSupportsUnsplitProcPoints", backendSupportsUnsplitProcPoints)


   , ("backendWantsBreakpointTicks", backendWantsBreakpointTicks)

   , ("backendSupportsEmbeddedBlobs", backendSupportsEmbeddedBlobs)

   , ("isValid . backendSimdValidity", isValid . backendSimdValidity)

   , ("backendSptIsDynamic", backendSptIsDynamic)

   , ("backendSupportsInterfaceWriting", backendSupportsInterfaceWriting)

   , ("backendRespectsSpecialise", backendRespectsSpecialise)


   ]
 where ncgFlavor NcgPrimitives = True
       ncgFlavor _ = False
       llvmFlavor LlvmPrimitives = True
       llvmFlavor _ = False
