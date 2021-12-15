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

import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable
import GHC.Utils.Panic

class MonadFail m => ControlTestMonad m where
  evalPredicate :: Label -> m Bool
  evalEnum      :: Label -> (Integer,Integer) -> m Integer
                   -- ^ range is half-open: includes low end but not high
  takeAction    :: Label -> m ()


data Event = Predicate Label Bool
           | Switch Label (Integer,Integer) Integer
           | Action Label
  deriving (Eq)

instance Show Event where
  show (Action l) = labelString l
  show (Predicate l b) = labelString l ++ "(" ++ (if b then "T" else "F") ++ ")"
  show (Switch l (lo,hi) i) =  labelString l ++ "(" ++ show i ++ " in [" ++ show lo ++ "," ++ show hi ++ "])"

labelString :: Label -> String
labelString = showSDocUnsafe . ppr

traceBits :: [Event] -> [Bool]
traceBits (Predicate _ b : events) = b : traceBits events
traceBits (Action _ : events) = traceBits events
traceBits (Switch _ (lo, hi) i : events) =
    inverseRangeSelect (lo, hi) i ++ traceBits events
traceBits [] = []

data FinalState a = Produced { pastEvents :: [Event], value :: a }
                  | Halted { pastEvents :: [Event] }
                  | Failed { pastEvents :: [Event], msg :: String }

instance Show a => Show (FinalState a) where
  show (Produced events a) = show events ++ " -> " ++ show a
  show (Halted events) = show events ++ " EXHAUSTS"
  show (Failed events msg) = show events ++ "  FAILED: " ++ msg

reverseEvents :: FinalState a -> FinalState a
reverseEvents (Produced events a) = Produced (reverse events) a
reverseEvents (Halted events) = Halted (reverse events)
reverseEvents (Failed events msg) = Failed (reverse events) msg


data BitConsuming a = BC { unBC :: [Bool] -> [Event] -> (FinalState a, [Bool]) }


--event :: Event -> BitConsuming ()
--event e = BC $ \bits past -> (Produced (e : past) (), bits)

instance Functor BitConsuming where
  fmap f ma = do { a <- ma; return $ f a }

instance Applicative BitConsuming where
  pure a = BC $ \bits past -> (Produced past a, bits)
  mf <*> ma = do { f <- mf; a <- ma; return $ f a }

instance Monad BitConsuming where
  m >>= k = BC $ \bits past ->
                 case unBC m bits past of
                   (Produced past' a, bits') -> unBC (k a) bits' past'
                   (Halted past, bits') -> (Halted past, bits')
                   (Failed past msg, bits') -> (Failed past msg, bits')
                   
runWithBits :: BitConsuming a -> [Bool] -> FinalState a
-- ^ Run with Booleans determining decisions, return final
-- state with most recent event first
runWithBits m bits = fst $ unBC m bits []

instance MonadFail BitConsuming where
  fail msg = BC $ \bits past -> (Failed past msg, bits)



instance ControlTestMonad BitConsuming where
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

