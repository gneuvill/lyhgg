module Chap6.Hofs where

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

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (sq 2) [1..])))

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

-- lambdas

numLongChains :: Int -- see targetChains
numLongChains = length (filter (\xs -> length xs > 15) (map chain ([1..100]::[Int])))

-- folds

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f b (a:as) = foldl' f (f b a) as

sum' :: Num b => [b] -> b
sum' = foldl' (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' a = foldl' (\bool a2 -> bool || a2 == a) False

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' (\a l -> f a : l) []

sum'' :: [Integer] -> Integer
sum'' = foldl1 (+)

maximum' :: Ord a => [a] -> a
maximum' = foldl1 (\a1 a2 -> if a1 > a2 then a1 else a2)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\a acc -> if p a then a:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\a _ -> a)

last' :: [a] -> a
last' = foldl1 (\_ a -> a)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application with $

cplxExpr :: Integer
cplxExpr = sum (filter (> 10) (map (*2) [2..10]))

smplExpr :: Integer
smplExpr = sum $ filter (>10) $ map (*2) [2..10]

-- function application is a... function !

apOnFunList :: [Int]
apOnFunList = map ($ 3) [(4+), (10*), (2^), (`mod` 2)]

-- function composition

negNums :: [Integer] -> [Integer]
negNums = map (\x -> negate $ abs x)
-- now using function composition
negNums' :: [Integer] -> [Integer]
negNums' = map (negate . abs)

-- function composition is right-associative, so we can compose many functions at a time

negSumTailNums :: [[Integer]] -> [Integer]
negSumTailNums = map (\xs -> negate $ sum $ tail xs)
-- now using function composition
negSumTailNums' :: [[Integer]] -> [Integer]
negSumTailNums' = map (negate . sum . tail)

-- point-free ( or pointless) style

fn :: Double -> Integer
fn x = ceiling (negate (tan (cos (max 50 x))))
-- can be rewritten as
fn' :: Double -> Integer
fn' = ceiling . negate . tan . cos . max 50

-- beware of readability

oddSquareSum' :: Integer
oddSquareSum' = sum $ takeWhile (<10000) $ filter odd $ map (sq 2) [1..]

oddSquareSum'' :: Integer
oddSquareSum'' = sum . takeWhile (<10000) . filter odd . map (sq 2) $ [1..]

-- both of them better rewritten as

oddSquareSum''' :: Integer
oddSquareSum''' =
  let oddSquares = filter odd . map (sq 2) $ [1..]
      lessThanTenThousand = takeWhile (<10000)
  in sum . lessThanTenThousand $ oddSquares
