module Chap8.Lockers where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Lockers = Map.Map Int (LockerState, String)

getLockerNum :: Int -> Lockers -> Either String String
getLockerNum i l =
  let
    result = Map.lookup i l
  in
   case result of Nothing ->
                    Left $ "Locker number " ++ show i ++ " doesn't exists !"
                  Just (Taken, _) ->
                    Left $ "Locker number " ++ show i ++ " is already taken !"
                  Just (Free, code) -> Right code

getLockerNum' :: Int -> Lockers -> Either String String   
getLockerNum' i l =
  treatResult (Map.lookup i l)
  where
    treatResult Nothing = Left $ "Locker number " ++ show i ++ " doesn't exists !"
    treatResult (Just (Taken, _)) = Left $ "Locker number " ++ show i ++ " is already taken !"
    treatResult (Just (Free, code)) = Right code

