{-# LANGUAGE InstanceSigs #-}

module Block3.Task2 where

-- `Semigroup` `NonEmpty` is in the Block2.Task2 --

-- | Data type which can store one or two values
data ThisOrThat a b = This a
    | That b
    | Both a b
    deriving (Show, Eq)

-- | `Semigroup` realisation for ThisOrThat
instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (This c)     = This $ (<>) a c
  (<>) (This a) (That d)     = Both a d
  (<>) (This a) (Both c d)   = Both ((<>) a c) d
  (<>) (That b) (This c)     = Both c b
  (<>) (That b) (That d)     = That $ (<>) b d
  (<>) (That b) (Both c d)   = Both c $ (<>) b d
  (<>) (Both a b) (This c)   = Both ((<>) a c) b
  (<>) (Both a b) (That d)   = Both a $ (<>) b d
  (<>) (Both a b) (Both c d) = Both ((<>) a c) $ (<>) b d

-- | Data type which can store `String`
-- or nothing
data Name = Name String
    | Empty
    deriving (Show, Eq)

-- | `Semigroup` realisation for `Name`
instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  Empty <> Name a = Name a
  a <> Empty = a
  Name "" <> a = Empty <> a
  a <> Name "" = a <> Empty
  Name a <> Name b = Name $ a ++ "." ++ b

-- | `Monoid` realisation for `Name`
instance Monoid Name where
  mempty :: Name
  mempty = Empty

-- | Newtype for hard version
newtype Endo a =
  Endo
    { getEndo :: a -> a
    }

-- | `Semigroup` realisation for `Endo`
instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  Endo a <> Endo b = Endo $ a . b

-- | `Monoid` realisation for `Endo`
instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
