-- Baby functions

module Chap2.Baby where

doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleUs2 x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- List comprehensions

boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase s = [c | c <- s, c `elem` ['A'..'Z']]

filterEven xs = [x | x <- xs, even x]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

result = [filterEven xs | xs <- xxs]

-- Tuples

triangles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b]]

rightTriangles = [(a, b, c) | (a, b, c) <- triangles, a^2 + b^2 == c^2]

perim24 = [(a, b, c) | (a, b, c) <- rightTriangles, a + b + c == 24]
