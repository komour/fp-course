{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc =
  let oneSide :: Either a (Either b c) -> Either (Either a b) c
      oneSide (Left a)          = Left $ Left a
      oneSide (Right (Right c)) = Right c
      oneSide (Right (Left b))  = Left $ Right b
      
      anotherSide :: Either (Either a b) c -> Either a (Either b c)
      anotherSide (Left (Left a))  = Left a
      anotherSide (Left (Right b)) = Right $ Left b
      anotherSide (Right c)        = Right $ Right c
   in (oneSide, anotherSide)
