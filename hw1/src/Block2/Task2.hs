{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block2.Task2 where

-- Foldable Tree instance is in the Block1.Task3

-- | Data type for non empty lists
-- deriving Eq here's for tests only
data NonEmpty a = a :| [a]
    deriving (Show, Eq)

-- | Converting list to NonEmpty
getNonEmpty :: [a] -> NonEmpty a
getNonEmpty (x:xs) = x :| xs
getNonEmpty _      = error "empty list in getNonEmpty"

-- | Splitting the list into sub-lists by the specified element
splitOn :: forall a . (Eq a) => a -> [a] -> NonEmpty [a]
splitOn delim = foldr merge (getNonEmpty [[]])
  where
    merge :: a -> NonEmpty [a] -> NonEmpty [a]
    merge cur (x :| xs)
      | cur == delim = getNonEmpty $ [] : x : xs
      | otherwise = getNonEmpty $ (cur : x) : xs

-- | Inverse function for `splitOn`
joinWith :: a -> [[a]] -> [a]
joinWith ch list = init $ foldMap (++ [ch]) list


------------- Block3.Task2 -------------

-- | Semigroup realisation for NonEmpty
instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = (:|) x $ xs ++ [y] ++ ys
