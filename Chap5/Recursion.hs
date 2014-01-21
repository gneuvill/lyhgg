module Chap5.Recursion where

fibo :: Int -> [Int]
fibo a = [fib x | x <- [0..a]]
  where fib x
          | x == 0    = 0
          | x == 1    = 1
          | otherwise = fib (x - 1) + fib (x - 2)

maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _ | i <= 0 = []
take' _ [] = []
take' i (x:xs) = x:take' (i - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
_ `elem'` [] = False
a `elem'` (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smallerSorted ++ [x] ++ greaterSorted
  where
    smallerSorted = quickSort [s | s <- xs, s <= x]
    greaterSorted = quickSort [g | g <- xs, g > x]
