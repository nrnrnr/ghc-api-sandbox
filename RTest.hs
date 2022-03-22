{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import System.Environment ( getArgs )

import GHC
import GHC.Driver.Session
--import GHC.Utils.Outputable
import Control.Monad.IO.Class

import qualified GHC.LanguageExtensions as LangExt

import LoadCmmGroup


main :: IO ()
main = do
    libdir:files <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        raw_dflags <- getSessionDynFlags
        let dflags = raw_dflags `xopt_set` LangExt.MagicHash
                         `xopt_set` LangExt.StandaloneKindSignatures
                         `xopt_set` LangExt.UnliftedDatatypes
                         `xopt_set` LangExt.DataKinds
        setSessionDynFlags dflags
        -- let sdctx = initSDocContext dflags defaultUserStyle
        groups <- mapM loadPath files
        liftIO $ putStrLn $ "loaded " ++ count groups "group"

count :: [a] -> String -> String
count xs thing = case length xs of
                   1 -> "1 " ++ thing
                   n -> show n ++ " " ++ thing ++ "s"
