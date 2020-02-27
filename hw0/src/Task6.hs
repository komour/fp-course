module Task6
  ( p1
  , p2
  ) where

import           Data.Maybe (mapMaybe)
import           Task1      (distributivity)

-- WHNF is
-- (Left "harold hide the pain", Left "harold hide the pain")
p1 :: (Either String b, Either String c)
p1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- WHNF is
-- False
p2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
