module Task5
  ( succChurch
  , churchMult
  , churchPlus
  -- , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f $ b f x

churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f x = a (b f) x

-- churchToInt :: Nat Integer -> Integer
-- churchToInt n = undefined
