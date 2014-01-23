module Chap8.Days where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum)

strMonday :: String
strMonday = show Monday -- Show

monday :: Day
monday = read strMonday :: Day -- Read

mondayIsMonday :: Bool
mondayIsMonday = monday == Monday -- Eq

tuesdayGTMonday :: Bool
tuesdayGTMonday = Tuesday > Monday -- Ord

tuesdayGTMonday' :: Ordering
tuesdayGTMonday' = Tuesday `compare` Monday -- Ord

beforeTuesday :: Day
beforeTuesday = pred Tuesday -- Enum

tuesdayAfterMonday :: Bool
tuesdayAfterMonday = succ Monday == Tuesday -- Enum

days :: [Day]
days = [Monday .. Sunday] -- Enum

minDay :: Day
minDay = minBound :: Day -- Bounded

maxDay :: Day
maxDay = maxBound :: Day -- Bounded

daysThroughBounds :: [Day]
daysThroughBounds = [minBound .. maxBound] :: [Day] -- Enum and Bounded
