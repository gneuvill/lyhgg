module Chap8.Vector where

data Vector a = Vector a a a deriving Show

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: Num a => Vector a -> Vector a -> a
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
