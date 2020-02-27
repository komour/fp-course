module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import           Data.Void (Void)

type Neg a = a -> Void

-- doubleNeg :: a -> ((a -> Void) -> Void)
doubleNeg :: a -> Neg (Neg a)
doubleNeg a = \func -> func a

-- excludedNeg :: ((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = aToVoidToVoid aToVoid
  where
--  aToVoidToVoid :: a -> Void -> Void
    aToVoidToVoid x = f $ Right x

--  aToVoid :: a -> Void
    aToVoid x = f $ Left x

-- can't be populated
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- can't be populated
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim =
  let contraposition :: (a -> b) -> Neg b -> Neg a
      contraposition f g x = g $ f x
   in contraposition doubleNeg
