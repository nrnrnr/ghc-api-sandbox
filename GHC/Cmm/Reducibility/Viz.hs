module GHC.Cmm.Reducibility.Viz
  ( CollapseEvent
  , collapseViz
  )
where


-- | Call this function to get a visualization of the Hecht/Ullman
-- collapsing algorithm, with splitting.  The visualization gives the
-- sequence of notable events, in order of occurrence.
collapseViz :: CmmGraph -> [CollapseEvent]
collapseViz = unViz . collapseInductiveGraph . cgraphOfCmm

-- | Steps that occur during the Hecht/Ullman algorithm.
data CollapseEvent
  = ConsumeBy Node Node CGraph -- ^ In the given graph, a node is
                               -- consumed (absorbed) by its unique
                               -- predecessor (and any resulting
                               -- self-edges are removed).

  | SplitAt CGraph Node        -- ^ The graph is split at the given node.

  | Finish CGraph              -- ^ The algorithm terminates,
                               -- producing the given graph.

-- | State for a state monad: all the events that have occurred, most
-- recent event first.
newtype VS = VS { unVS :: [CollapseEvent] }

unViz :: State VS a -> [CollapseEvent]
unViz m = reverse $ unVS $ execState m emptyVs

-- | Visualization monad.  Accumulates events.
newtype VM a = VM { _unVM :: State VS a }
  deriving (Applicative, Functor, Monad)

emptyVs :: VS
emptyVs = VS  []

instance MonadState VS VM where
  get = VM get
  put s = VM $ put s

instance VizCollapseMonad (State VS) Gr where
  consumeByInGraph to from g = modify (add $ ConsumeBy to from g)
  splitGraphAt g (k, _) = modify (add $ SplitAt g k)
  finalGraph g = modify (add $ Finish g)

add :: CollapseEvent -> VS -> VS
add event (VS events) = VS (event : events)
