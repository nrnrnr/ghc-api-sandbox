module Split
where

{- Note [Node splitting]

Definition: A control-flow edge A -> B is *bad* when B *precedes* A
in the reverse postorder numbering, but B does not *dominate* A.

Claim: A flow graph with no bad edges is reducible.
(A bad edge is a second entry point to a loop.)

Theorem: Bad edges cannot form a cycle.
Proof: Following a bad edge decreases the reverse postorder number.

Definition: A node is *bad* when at least one of the edges leaving it is bad.

To decrease the number of bad edges in an irreducible control-flow graph:

 1. Find a bad edge that points to a good node; call that edge A -> B

 2. Create a new node B' that is a copy of B.

 3. Replace the edge A -> B with edge A -> B'.

Claim: B' is a good node.
(What justifies this claim?  What do we know about the new numbering?)

Claim: The new edge A -> B' is good.

-}
