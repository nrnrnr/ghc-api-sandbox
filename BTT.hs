module Main where

import Data.List (intercalate)
import Data.Map (Map, insertWith, toList, empty)

import GHC.Driver.Backend
import GHC.Driver.Backend.Rep


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
   [ ("backendProducesObject", backendProducesObject)
   , ("backendNeedn'tLink", backendNeedn'tLink)
   , ("backendGeneratesCode", backendGeneratesCode)
   , ("backendInterfaceHasCodegen", backendInterfaceHasCodegen)
   , ("backendRetainsAllBindings", backendRetainsAllBindings)

   , ("backendWantsLlvmCppMacros", backendWantsLlvmCppMacros)
   , ("backendWantsClangTools", backendWantsClangTools)

   , ("backendNeedsFullWays", backendNeedsFullWays)

   , ("useNcgPrimitives", useNcgPrimitives)
   , ("useLlvmPrimitives", useLlvmPrimitives)


   , ("backendSupportsSwitch", backendSupportsSwitch)


   , ("backendSupportsStopC", backendSupportsStopC)

   , ("supportsHpc", supportsHpc)
   , ("needsPlatformNcgSupport", needsPlatformNcgSupport)

   , ("backendUnregisterisedOnly", backendUnregisterisedOnly)
   , ("canReplaceViaC", canReplaceViaC)
   , ("canBeReplacedByViaC", canBeReplacedByViaC)

   , ("backendForcesOptimization0", backendForcesOptimization0)
   , ("backendSplitsProcPoints", backendSplitsProcPoints)


   , ("backendWantsBreakpointTicks", backendWantsBreakpointTicks)

   , ("backendSupportsEmbeddedBlobs", backendSupportsEmbeddedBlobs)

   , ("backendSupportsSimd", backendSupportsSimd)

   , ("backendSptIsDynamic", backendSptIsDynamic)

   , ("backendInhibitsInterfaceWriting", backendInhibitsInterfaceWriting)

   , ("backendIgnoresSpecialise", backendIgnoresSpecialise)

   , ("backendWantsInterfaceFile", backendWantsInterfaceFile)


   ]

