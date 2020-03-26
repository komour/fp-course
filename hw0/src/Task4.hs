module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import           Data.Function   (fix)
import           Numeric.Natural

iterateElement :: a -> [a]
iterateElement a = fix (a :)

fibonacci :: Natural -> Natural
fibonacci n =
  let fibonacciCore :: (Natural -> Natural) -> Natural -> Natural
      fibonacciCore f c
        | n == 0 = 0
        | n == 1 = 1
        | otherwise = f (c - 1) + f (c - 2)
   in fix fibonacciCore n

factorial :: Integer -> Integer
factorial =
  let factorialCore :: (Integer -> Integer) -> Integer -> Integer
      factorialCore f n
        | n <= 1 = 1
        | otherwise = n * f (n - 1)
   in fix factorialCore

mapFix :: (a -> b) -> [a] -> [b]
mapFix =
  let mapCore :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
      mapCore f mapB t =
        case t of
          x:xs -> mapB x : f mapB xs
          []   -> []
   in fix mapCore
