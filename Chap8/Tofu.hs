module Chap8.Tofu where

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank (b a) deriving Show

frankField :: Frank a b -> b a
frankField (Frank f) = f

instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k} deriving Show

instance Functor (Barry t k) where
  fmap f (Barry y d) = Barry (f y) d
