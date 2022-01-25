module Main where

import Data.List (intercalate)
import Data.Map (Map, insertWith, toList, empty)

import GHC.Driver.Backend
import GHC.Driver.Backend.Rep
import GHC.Utils.Error

isValid :: Validity -> Bool
isValid IsValid = True
isValid (NotValid _) = False

main :: IO ()
main = do
  mapM_ putPair $ filter interesting $ toList info
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
   , ("backendNeedsLink", backendNeedsLink)
   , ("backendGeneratesCode", backendGeneratesCode)
   , ("backendRetainsAllBindings", backendRetainsAllBindings)

   , ("backendWantsClangTools", backendWantsClangTools)

   , ("backendNeedsFullWays", backendNeedsFullWays)

   , ("backendWantsNcgPrimitives", backendWantsNcgPrimitives)
   , ("backendWantsLlvmPrimitives", backendWantsLlvmPrimitives)


   , ("backendHasNativeSwitch", backendHasNativeSwitch)

   , ("backendCanExportCStatics", isValid . backendValidityOfCExportStatic)
   , ("backendCanImportC", isValid . backendValidityOfCImport)

   , ("backendSupportsStopC", backendSupportsStopC)

   , ("backendSupportsHpc", backendSupportsHpc)
   , ("backendNeedsPlatformNcgSupport", backendNeedsPlatformNcgSupport)

   , ("backendUnregisterisedOnly", backendUnregisterisedOnly)
   , ("backendSwappableWithViaC", backendSwappableWithViaC)

   , ("backendForcesOptimization0", backendForcesOptimization0)
   , ("backendSupportsUnsplitProcPoints", backendSupportsUnsplitProcPoints)


   , ("backendWantsBreakpointTicks", backendWantsBreakpointTicks)

   , ("backendSupportsEmbeddedBlobs", backendSupportsEmbeddedBlobs)

   , ("backendSupportsSimd", backendSupportsSimd)

   , ("backendSptIsDynamic", backendSptIsDynamic)

   , ("backendSupportsInterfaceWriting", backendSupportsInterfaceWriting)

   , ("backendRespectsSpecialise", backendRespectsSpecialise)


   ]

