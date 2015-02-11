
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import           Data.Monoid
import           Data.Tree
import           Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) =
  GL (e : es) (empFun e + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es f) (GL es' f') =
    GL (es ++ es') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t =
  f (rootLabel t) $ map (treeFold f) $ subForest t

-- combineGLs :: Employee -> [GuestList] -> GuestList

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = f . unzip
  where
    f (withSubs, withoutSubs) =
      (mappend (glCons e mempty) (mconcat withoutSubs), mconcat withSubs)
