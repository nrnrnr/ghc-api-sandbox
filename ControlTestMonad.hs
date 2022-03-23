{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlTestMonad
  ( ControlTestMonad(..)
  , Event(..)
  )
where

import GHC.Utils.Outputable

class (MonadFail m) => ControlTestMonad stmt exp m where
  evalPredicate :: exp -> m Bool
  evalEnum      :: exp -> (Integer,Integer) -> m Integer
                   -- ^ range is half-open: includes low end but not high
  takeAction    :: stmt -> m ()

data Event stmt exp = Predicate exp Bool
                    | Switch exp (Integer,Integer) Integer
                    | Action stmt
  deriving (Eq)

instance (Outputable e, Outputable s) => Outputable (Event e s) where
  ppr (Action l) = ppr l
  ppr (Predicate l b) = ppr l <+> parens (if b then "T" else "F")
  ppr (Switch l (lo,hi) i) =
      ppr l <+> parens (hcat [text $ show i, " in [", text $ show lo, "..", text $ show hi, "]"])

instance (Show e, Show s) => Show (Event e s) where
  show (Action l) = show l
  show (Predicate l b) = show l ++ "(" ++ (if b then "T" else "F") ++ ")"
  show (Switch l (lo,hi) i) =  show l ++ "(" ++ show i ++ " in [" ++ show lo ++ ".." ++ show hi ++ "])"
