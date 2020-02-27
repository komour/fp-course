module Task3
  ( composition
  , identity
  , contraction
  , permutation
  ) where

-- | S combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | K combinator
k :: a -> b -> a
k a _ = a

-- | I combinator
identity :: a -> a
identity = s k k

-- | B combinator
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

-- | C combinator
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (k (s (k s) k)) s) (k k)

-- | W combinator
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s k)
