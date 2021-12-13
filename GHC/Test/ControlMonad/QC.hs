module GHC.Test.ControlMonad.QC
where

import GHC.Test.ControlMonad

import Test.QuickCheck


leftInverse :: (Int, Int) -> Int -> Bool
leftInverse range i = rangeSelect range (inverseRangeSelect range i) == Just (i, [])


checkVerse :: IO ()
checkVerse = quickCheck $
             \lo (NonNegative leftWidth) (NonNegative rightWidth) ->
                 let width = leftWidth + rightWidth
                     limit = lo + width
                     k = lo + leftWidth
                 in  k < limit && width > 1 ==> leftInverse (lo, limit) k

rightInverse :: (Int, Int) -> [Bool] -> Property
rightInverse range bits =
    case rangeSelect range bits of
      Nothing -> property Discard
      Just (i, leftovers) -> property $ bits == inverseRangeSelect range i ++ leftovers


checkRightVerse :: IO ()
checkRightVerse = quickCheck $
             \lo (Positive width) bits -> rightInverse (lo, lo + width) bits
