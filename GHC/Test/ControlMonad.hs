{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Test.ControlMonad
  ( ControlTestMonad(..)
  , Event(..)
  , FinalState(..)
  , BitConsuming
  , runWithBits
  , reverseEvents

  , rangeSelect
  , inverseRangeSelect

  , traceBits

  )
where

import GHC.Utils.Outputable
import GHC.Utils.Panic

class (Eq label, MonadFail m) => ControlTestMonad label m where
  evalPredicate :: label -> m Bool
  evalEnum      :: label -> (Integer,Integer) -> m Integer
                   -- ^ range is half-open: includes low end but not high
  takeAction    :: label -> m ()


data Event label = Predicate label Bool
                 | Switch label (Integer,Integer) Integer
                 | Action label
  deriving (Eq)

instance Outputable l => Outputable (Event l) where
  ppr = text . show

instance Outputable l => Show (Event l) where
  show (Action l) = showPprUnsafe l
  show (Predicate l b) = showPprUnsafe l ++ "(" ++ (if b then "T" else "F") ++ ")"
  show (Switch l (lo,hi) i) =  showPprUnsafe l ++ "(" ++ show i ++ " in [" ++ show lo ++ ".." ++ show hi ++ "])"


traceBits :: [Event a] -> [Bool]
traceBits (Predicate _ b : events) = b : traceBits events
traceBits (Action _ : events) = traceBits events
traceBits (Switch _ (lo, hi) i : events) =
    inverseRangeSelect (lo, hi) i ++ traceBits events
traceBits [] = []

data FinalState label a = Produced { pastEvents :: [Event label], value :: a }
                  | Halted { pastEvents :: [Event label] }
                  | Failed { pastEvents :: [Event label], msg :: String }

instance (Outputable label, Show a) => Show (FinalState label a) where
  show (Produced events a) = show events ++ " -> " ++ show a
  show (Halted events) = show events ++ " EXHAUSTS"
  show (Failed events msg) = show events ++ "  FAILED: " ++ msg

reverseEvents :: FinalState label a -> FinalState label a
reverseEvents (Produced events a) = Produced (reverse events) a
reverseEvents (Halted events) = Halted (reverse events)
reverseEvents (Failed events msg) = Failed (reverse events) msg


newtype BitConsuming label a =
    BC { unBC :: [Bool] -> [Event label] -> (FinalState label a, [Bool]) }


--event :: Event -> BitConsuming ()
--event e = BC $ \bits past -> (Produced (e : past) (), bits)

instance Functor (BitConsuming label) where
  fmap f ma = f <$> ma

instance Applicative (BitConsuming label) where
  pure a = BC $ \bits past -> (Produced past a, bits)
  mf <*> ma = do { f <- mf; f <$> ma; }

instance Monad (BitConsuming label) where
  m >>= k = BC $ \bits past ->
                 case unBC m bits past of
                   (Produced past' a, bits') -> unBC (k a) bits' past'
                   (Halted past, bits') -> (Halted past, bits')
                   (Failed past msg, bits') -> (Failed past msg, bits')

runWithBits :: BitConsuming label a -> [Bool] -> FinalState label a
-- ^ Run with Booleans determining decisions, return final
-- state with most recent event first
runWithBits m bits = fst $ unBC m bits []

instance MonadFail (BitConsuming label) where
  fail msg = BC $ \bits past -> (Failed past msg, bits)



instance Eq label => ControlTestMonad label (BitConsuming label) where
  evalPredicate lbl =
      BC $ \bits past -> case bits of
                           bit : bits' -> (Produced (Predicate lbl bit : past) bit, bits')
                           [] -> (Halted past, bits)

  evalEnum lbl range =
      BC $ \bits past -> case rangeSelect range bits of
                           Just (i, bits') -> (Produced (Switch lbl range i : past) i, bits')
                           Nothing -> (Halted past, bits)

  takeAction lbl = BC $ \bits past -> (Produced (Action lbl : past) (), bits)


rangeSelect :: (Integer, Integer) -> [Bool] -> Maybe (Integer, [Bool])
rangeSelect (lo, limit) bits | lo == pred limit = Just (lo, bits)
rangeSelect _ [] = Nothing
rangeSelect (lo, limit) (bit : bits) =
    rangeSelect (if bit then (lo, mid) else (mid, limit)) bits
  where mid = (lo + limit) `div` 2

inverseRangeSelect :: (Integer, Integer) -> Integer -> [Bool]
inverseRangeSelect (lo, limit) i
    | lo == pred limit = if i == lo then [] else panic "inverseRangeSelect"
    | otherwise = if i < mid then True : inverseRangeSelect (lo, mid) i
                  else False : inverseRangeSelect (mid, limit) i
  where mid = (lo + limit) `div` 2
