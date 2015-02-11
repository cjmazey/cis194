
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import           Data.Monoid
import           Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) =
  GL (e : es) (empFun e + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es f) (GL es' f') =
    GL (es <> es') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
