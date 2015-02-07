module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream h t) = Stream (f h) (fmap f t)

streamToList :: Stream a -> [a]
streamToList (Stream h t) = h : streamToList t

instance Show a => Show (Stream a) where
  show s = "Stream [" ++
           concatMap ((++ ",") . show) ((take 20 . streamToList) s) ++
           ". . .]"

streamRepeat :: a -> Stream a
streamRepeat e = Stream e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Stream e $ streamFromSeed f (f e)
