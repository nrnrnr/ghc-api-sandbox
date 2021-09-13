module Main where

import Control.Monad.IO.Class (liftIO)
import DynFlags ( defaultFatalMessager, defaultFlushOut )
import GHC
import GHC.IO.Handle.FD (stdout)
import GHC.Paths ( libdir )
import Outputable (printSDocLn, ppr, defaultUserStyle)
import Pretty (Mode(PageMode))
import System.Environment ( getArgs )
 
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

corify :: String -> GHC.Ghc CoreModule
corify pathname =
  do coremod <- compileToCoreSimplified pathname
     dflags <- getSessionDynFlags
     let doc = ppr (cm_binds coremod)
     liftIO $ printSDocLn PageMode dflags stdout (defaultUserStyle dflags) doc
     return coremod
