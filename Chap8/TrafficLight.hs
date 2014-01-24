module Chap8.TrafficLight where

data TrafficLight = Red | Yellow | Green

-- instead of derivation (using the 'derive' keyword)
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Green = "Green Light"
  show Yellow = "Yellow Light"

                              
