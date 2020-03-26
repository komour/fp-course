{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 where

-- | Data type for natural numbers
data Nat = Z
    | S Nat
    deriving (Show)

-- | Checking natural numbers for equality
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z = True
  S x == S y = x == y
  _ == _ = False

-- | Comparison of natural numbers
instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  Z <= _ = True
  S x <= S y = x <= y
  S _ <= Z = False

-- | Addition of two natural numbers
nsum :: Nat -> Nat -> Nat
Z `nsum` x = x
x `nsum` Z = x
S x `nsum` S y = S $ S $ x `nsum` y

-- | Subtraction of natural numbers
sub :: Nat -> Nat -> Nat
Z `sub` _ = Z
x `sub` Z = x
S x `sub` S y = x `sub` y

-- | Multiplication of two natural numbers
mul :: Nat -> Nat -> Nat
Z `mul` _ = Z
S x `mul` y = x `mul` y `nsum` y

-- | Converting Integers to Naturals
fromInt :: Integer -> Nat
fromInt 0 = Z
fromInt x = S $ fromInt $ x - 1

-- | Converting Naturals to Integers
toInt :: Nat -> Int
toInt Z     = 0
toInt (S x) = toInt x + 1

-- | Parity check for Naturals
isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S x)) = isEven x

-- | Integer division of natural numbers
ndiv :: Nat -> Nat -> Nat
_ `ndiv` Z = error "dividing by zero"
x `ndiv` y =
  if x < y
    then Z
    else S $ ndiv (x `sub` y) y

-- | The remainder of dividing a natural number by another
nmod :: Nat -> Nat -> Nat
_ `nmod` Z = error "dividing by zero"
nmod x y =
  if x < y
    then x
    else nmod (x `sub` y) y
