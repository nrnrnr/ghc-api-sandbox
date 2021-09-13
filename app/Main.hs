module Main where

import GHC
import GHC.Paths ( libdir )
import DynFlags ( defaultFatalMessager, defaultFlushOut )
import System.Environment ( getArgs )
 
main :: IO SuccessFlag
main = do
    args <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        flags <- mapM translate args
        return $ mconcat flags
--        target <- guessTarget "test_main.hs" Nothing
--        setTargets [target]
--        load LoadAllTargets

instance Semigroup SuccessFlag where
  (<>) Succeeded Succeeded = Succeeded
  (<>) _ _ = Failed

instance Monoid SuccessFlag where
  mempty = Succeeded

translate :: String -> GHC.Ghc SuccessFlag
translate pathname = do
  target <- guessTarget pathname Nothing
  setTargets [target]
  load LoadAllTargets
