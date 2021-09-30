module Simple.ImportStg (
  stgToSimpleStg
  )
where

import Data.Maybe

import GHC.Stg.Syntax

import qualified Simple.Stg as S

stgToSimpleStg :: [GenStgTopBinding pass] -> S.Program
stgToSimpleStg = S.Program . mapMaybe topBind

topBind (StgTopStringLit {}) = Nothing
topBind (StgTopLifted b) = Just $ binding b

binding = undefined

  
