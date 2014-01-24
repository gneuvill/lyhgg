module Chap8.MyList where

infixr 5 :-:
data MyList a = Empty | a :-: MyList a deriving (Show, Read, Eq, Ord)

(.++) :: MyList a -> MyList a -> MyList a
Empty .++ l2 = l2
(x :-: xs) .++ l2 = x :-: (xs .++ l2)
