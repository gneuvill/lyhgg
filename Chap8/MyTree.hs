module Chap8.MyTree where

-- a 'normal' (possibly unbalanced) binary search tree
data MyTree a = NilTree | Node a (MyTree a) (MyTree a) deriving (Show, Read, Eq)

leaf :: a -> MyTree a
leaf a = Node a NilTree NilTree

insertTree :: Ord a => a -> MyTree a -> MyTree a
insertTree a NilTree = leaf a
insertTree a t @ (Node x left right)
  | a > x = Node x left $ insertTree a right
  | a < x = Node x (insertTree a left) right
  | otherwise = t

elemTree :: Ord a => a -> MyTree a -> Bool
elemTree _ NilTree = False
elemTree a (Node x left right)
  | a == x = True
  | a > x = a `elemTree` right
  | otherwise = a `elemTree` left

treeFromList :: Ord a => [a] -> MyTree a
treeFromList = foldr insertTree NilTree
