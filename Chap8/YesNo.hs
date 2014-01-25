module Chap8.YesNo where

import Chap8.MyTree
import Chap8.TrafficLight

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno _ = True

instance YesNo (MyTree a) where
  yesno NilTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: YesNo a => a -> t -> t -> t
yesnoIf ynv yr nr = if yesno ynv then yr else nr
