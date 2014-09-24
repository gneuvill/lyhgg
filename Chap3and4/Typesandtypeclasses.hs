module Chap3and4.TypesAndTypeClasses where

-- Type signatures

-- removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Pattern matching (in function definitions)

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN !"
lucky _ = "Sorry, you're out of luck pal !"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "head on empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The empty list"
tell (x:[]) = "A one element list. The element is " ++ show x
tell (x:y:[]) = "A two element list. The first element is " ++ show x ++ " and the second " ++ show y
-- tell [x] = "A one element list. The element is " ++ show x
-- tell [x, y] = "A two element list. The first element is " ++ show x ++ " and the second " ++ show y
tell (x:y:xs) = "A list with two or more elements. The first element is " ++ show x ++ " and the second " ++ show y ++ ". The remainder of the list is " ++ show xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 'as' pattern
capital :: String -> String
capital "" = "Empty string"
capital l@(x:_) = "The first letter of " ++ l ++ " is " ++ [x]

-- pattern guards

bmiTell :: RealFloat a => a -> String
bmiTell bmi
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "overweight"
  | otherwise = "obese"

bmiTell' :: RealFloat a => a -> a -> String
bmiTell' weight height
  | weight / height^(2::Int) <= 18.5 = "underweight"
  | weight / height^(2::Int) <= 25.0 = "normal"
  | weight / height^(2::Int) <= 30.0 = "overweight"
  | otherwise = "obese"

max' :: Ord a => a -> a -> a
max' a b
  | a < b = b
  | otherwise = a

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
  | a < b     = LT
  | a == b    = EQ
  | otherwise = GT

-- 'where' clause

bmiTell'' :: RealFloat a => a -> a -> String
bmiTell'' weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fat    = "overweight"
  | otherwise     = "obese"
  where bmi = weight / height^(2::Int)
        (skinny, normal, fat) = (18.5, 25.0, 30.0) -- pattern matching in a where clause

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height^(2::Int)

-- 'let ... in' expressions

calcBmis' :: RealFloat a => [(a, a)] -> [a]
calcBmis' xs =
  let bmi weight height = weight / height^(2::Int)
  in [bmi w h | (w, h) <- xs]

calcBmis'' :: RealFloat a => [(a, a)] -> [a]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h^(2::Int)]

-- 'case ... of' expressions (other form of pattern matching)

head'' :: [a] -> a
head'' xs = case xs of [] -> error "Head of empty list"
                       x:_ -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [_] -> "a singleton list"
                                               _ -> "a longer list"

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  -- pattern matching to define the 'what' function
  where what [] = "empty"
        what [_] = "a singleton list"
        what _ = "a longer list"
