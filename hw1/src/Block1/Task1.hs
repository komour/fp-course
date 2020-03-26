{-# LANGUAGE InstanceSigs #-}

module Block1.Task1 where

-- | Data type to swtore days of week
data DaysWeek = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    deriving (Show)


-- | Returns the number of the day
getDayNumber :: DaysWeek -> Int
getDayNumber Sunday    = 0
getDayNumber Monday    = 1
getDayNumber Tuesday   = 2
getDayNumber Wednesday = 3
getDayNumber Thursday  = 4
getDayNumber Friday    = 5
getDayNumber Saturday  = 6

-- | Checks whether one weekDay is equal to another
instance Eq DaysWeek where
  (==) :: DaysWeek -> DaysWeek -> Bool
  a == b = getDayNumber a == getDayNumber b

-- | Returns the day by its number
getDayByNumber :: Int -> DaysWeek
getDayByNumber 0 = Sunday
getDayByNumber 1 = Monday
getDayByNumber 2 = Tuesday
getDayByNumber 3 = Wednesday
getDayByNumber 4 = Thursday
getDayByNumber 5 = Friday
getDayByNumber 6 = Saturday
getDayByNumber _ = error "illegal argument"


-- | Returns the next day after some days
afterDays :: DaysWeek -> Int -> DaysWeek
afterDays curDay offset = getDayByNumber $ (getDayNumber curDay + offset) `mod` 7

-- | Returns the next day for the day passed in arguments
nextDay :: DaysWeek -> DaysWeek
nextDay curDay = afterDays curDay 1

-- | Checks wheather current day is weekend or not
isWeekend :: DaysWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Returns the number of days remaining until Friday
daysToParty :: DaysWeek -> Int
daysToParty Saturday = 6
daysToParty curDay   = 5 - getDayNumber curDay



