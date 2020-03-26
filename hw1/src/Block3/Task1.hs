{-# LANGUAGE InstanceSigs #-}

module Block3.Task1 where

import           Data.Either (partitionEithers)

-- | Accepts a list of lists inside Maybe and
-- returns the concatenation of all internal lists.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr core []
  where
    core Nothing acc     = acc
    core (Just list) acc = list ++ acc

-- | Accepts an arbitrary set of Either,
-- where both `Left` and `Right` contain some monoidal elements,
-- returns a couple from the results of monoidal union
-- of separately elements inside `Left` and separately elements inside `Right`
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat list = (mconcat left, mconcat right)
  where
    (left, right) = partitionEithers list
