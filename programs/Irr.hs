module Irr
where

data List a = Nil | Cons a (List a)

--length' :: Bool -> List a -> Int
--length' trigger xs = if trigger then countA 0 xs else countB 0 xs
--  where countA n Nil = case n of m -> m
--        countA n (Cons _ as) = case n + 1 of m -> case countB m as of k -> k
--        countB n Nil = case n of m -> m
--        countB n (Cons _ as) = case n + 2 of m -> case countA m as of k -> k

length'' :: Bool -> List a -> Int
length'' trigger xs = if trigger then countA 0 xs else countB 0 xs
  where countA n Nil = case n of m -> m
        countA n (Cons _ as) = case n + 1 of m -> countB m as
        countB n Nil = case n of m -> m
        countB n (Cons _ as) = case n + 2 of m -> countA m as
