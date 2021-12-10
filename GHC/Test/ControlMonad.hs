module GHC.Test.ControlMonad 
--  ( ControlTestMonad(..)
--  , Event(..)
--
--
--
--
--  )
where

import GHC.Cmm.Dataflow.Label

class MonadFail m => ControlTestMonad m where
  evalPredicate :: Label -> m Bool
  evalEnum      :: Label -> (Int,Int) -> m Int
                   -- ^ range is half-open: includes low end but not high
  takeAction    :: Label -> m ()


data Event = Predicate Label Bool | Switch Label (Int,Int) Int | Action Label

{-
data State = Running { oldEvents :: [Event], futureDecisions :: [Bool] }
           | Halted { oldEvents :: [Event] }
           | Failed { oldEvents :: [Event], msg :: String }

data BitsConsumingTest a = BCT { unBCT :: State -> (State, a) }

instance Functor BitsConsumingTest where
  fmap f ma = do { a <- ma; return $ f a }

instance Applicative BitsConsumingTest where
  pure a = BCT $ \s -> (s, a)
  mf <*> ma = do { f <- mf; a <- ma; return $ f a }

instance Monad BitsConsumingTest where
  m >>= k = BCT $ \s -> let (s', a) = unBCT m s
                        in  unBCT (k a) s'


                         
--instance MonadFail BitsConsumingTest where
--  fail s = BCT $ withRunning (\events _ -> Failed events msg)


withRunning :: ([Event] -> [Bool] -> State) -> State -> State
withRunning k (Running events bs) = k events bs
withRunning _ state = state
-}

data FinalState a = Produced { pastEvents :: [Event], value :: a }
                  | Halted { pastEvents :: [Event] }
                  | Failed { pastEvents :: [Event], msg :: String }

data BitConsuming a = BC { unBC :: [Bool] -> [Event] -> (FinalState a, [Bool]) }


event :: Event -> BitConsuming ()
event e = BC $ \bits past -> (Produced (e : past) (), bits)

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


rangeSelect :: (Int, Int) -> [Bool] -> Maybe (Int, [Bool])
rangeSelect (lo, limit) bits | lo == pred limit = Just (lo, bits)
rangeSelect _ [] = Nothing
rangeSelect (lo, limit) (bit : bits) =
    rangeSelect (if bit then (lo, mid) else (mid, limit)) bits
  where mid = (lo + limit) `div` 2
