module Chap8.Person where

-- normal syntax...

data JPerson = JPerson String String Int Float String String deriving Show

-- ... with hand-written getters ! (horrible)

jfirstName :: JPerson -> String
jfirstName (JPerson fn _ _ _ _ _) = fn

-- etc, etc...

-- Better now with record syntax

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving Show

data Car = Car {company :: String, model :: String, year :: Int} deriving Show

tellCar :: Car -> String
tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data SmallPers = SmallPers { fName :: String
                           , lName :: String
                           , old :: Int
                           } deriving (Eq, Show, Read)

robert :: String
robert = show $ SmallPers "Robert" "Dupont" 46

ourRobert :: SmallPers
ourRobert = read robert
-- without type declaration, we'd have to write 'read robert :: SmallPers'

robertIsOurRobert :: Bool
robertIsOurRobert = read robert == ourRobert -- here, the type of 'read robert' is inferred

