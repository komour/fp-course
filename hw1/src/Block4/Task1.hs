module Block4.Task1 where

import           Text.Read (readMaybe)

-- | Sums the numbers in the string
-- returns `Maybe`
stringSum :: String -> Maybe Int
stringSum = (sum <$>) . traverse readMaybe . words
