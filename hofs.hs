module Hofs where

-- curried and partially applied functions

multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: Integer -> Integer -> Integer
multTwoWithNine = multThree 9

multWithEighteen :: Integer -> Integer
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x
-- better written as
compareWithHundred = compare 100

-- partially applied infix functions (using sections)

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

-- higher-orderism

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g y x = f x y

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : fxs
  | otherwise = fxs
  where fxs = filter' p xs

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = smallerSorted ++ [x] ++ greaterSorted
  where
    smallerSorted = quickSort' (filter' (<=x) xs)
    greaterSorted = quickSort' (filter' (>x) xs)

largestDivisible :: Integer
-- largestDivisible = maximum (filter p [0..100000])
--   where p x = x `mod` 3829 == 0
-- better
largestDivisible = head (filter p [100000, 99999..])
   where p x = x `mod` 3829 == 0

sq :: Integer -> Integer -> Integer
sq = (^)

oddSquares :: Integer
oddSquares = sum (takeWhile (<10000) (filter odd (map (sq 2) [1..])))

-- Collatz sequences

chain :: Integral a => a -> [a]
chain 1 = [1]
chain x = x : chain y
  where y = if even x then x `div` 2 else x * 3 + 1

targetChains :: Int
targetChains = length (filter p (map chain oneToHundred))
  where
    oneToHundred = [1..100]::[Int]
    p xs = length xs > 15

-- list of functions

listOfFuns :: [Integer -> Integer]
listOfFuns = map (*) [0..]
-- the expression "(listOfFuns !! 4) 5" yields 20

-- Lambdas !

-- see targetChains
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain ([1..100]::[Int])))
