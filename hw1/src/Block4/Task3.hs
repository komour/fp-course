{-# LANGUAGE InstanceSigs #-}

module Block4.Task3 where

import           Control.Applicative (liftA2)

-- | Data type for non empty lists
data NonEmpty a = a :| [a]
    deriving (Show)

-- | Converts list to NonEmpty
fromList :: [a] -> NonEmpty a
fromList (x:xs) = x :| xs
fromList []     = error "empty list in \"fromList :: [a] -> NonEmpty a\""

-- | Converts NonEmpty to list
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

-- | `Functor` realisation for `NonEmpty`
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| map f xs

-- | `Applicative` realisation for `NonEmpty`
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = (:| [])

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  func <*> neList = fromList $ f <*> list
    where
      f = toList func
      list = toList neList

-- | `Monad` realisation for `NonEmpty`
instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return = pure
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x :| xs) >>= f = (:|) y $ ys ++ zs
    where
      y :| ys = f x
      zs = xs >>= toList . f

-- | `Foldable` realisation for `NonEmpty`
instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (x :| xs) = x `f` foldr f acc xs

-- | `Traversable` realisation for `NonEmpty`
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = liftA2 (:|) (f x) $ traverse f xs
