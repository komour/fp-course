{-# LANGUAGE InstanceSigs #-}

module Block4.Task2 where

-- | Data type for binary tree
data Tree a = Branch (Tree a) (Tree a)
    | Leaf a
    deriving Show

-- | `Functor` realisation for `Tree`
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)            = Leaf $ f a
  fmap f (Branch left right) = Branch (fmap f left) $ fmap f right

-- | `Foldable` realisation for `Tree`
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (Leaf a)            = f a
  foldMap f (Branch left right) = foldMap f left <> foldMap f right

-- | `Traversable` realisation for `Tree`
instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch left right) =
    Branch <$> traverse f left <*> traverse f right

-- | `Applicative` realisation for `Tree`
instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> a = fmap f a
  Branch left right <*> a = Branch (left <*> a) $ right <*> a
