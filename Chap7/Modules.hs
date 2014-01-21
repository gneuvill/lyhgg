module Chap7.Modules where

import Data.List (nub, tails, find)
import qualified Data.Map as Map

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

-- behaves like isInfixOf
search :: Eq a => [a] -> [a] -> Bool
search needle haystack =
  foldr (\as bool -> subSuffix as == needle || bool) False $ tails haystack
  where
    nlen = length needle
    subSuffix as = take nlen as

-- Data.Map

-- will crash on empty list
findKey :: Eq a => a -> [(a, c)] -> c
findKey key xs = snd . head . filter (\(k, _) -> key == k) $ xs

-- more secure, using Maybe and standard recursion
findKey' :: Eq a => a -> [(a, b)] -> Maybe b
findKey' _ [] = Nothing
findKey' key ((k, v) : _) = if key == k
                            then Just v
                            else Nothing

-- better rewritten as
findKey'' :: Eq a => a -> [(a, b)] -> Maybe b
findKey'' key = foldl (\opt (k, v) -> if k == key then Just v else opt) Nothing

-- and better again (not in the book though)
findKey''' :: Eq a => a -> [(a, b)] -> Maybe b
findKey''' key xs = fmap snd $ find ((== key) . fst) xs

fromList' :: Ord k => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty
