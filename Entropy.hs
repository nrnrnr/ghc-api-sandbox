
traceBits :: [Event a b] -> [Bool]
traceBits (Predicate _ b : events) = b : traceBits events
traceBits (Action _ : events) = traceBits events
traceBits (Switch _ (lo, hi) i : events) =
    inverseRangeSelect (lo, hi) i ++ traceBits events
traceBits [] = []
