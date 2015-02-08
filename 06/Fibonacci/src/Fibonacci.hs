{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Streams

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = "Stream [" ++
           concatMap ((++ ",") . show) ((take 20 . streamToList) s) ++
           ". . .]"

streamToList :: Stream a -> [a]
streamToList (Stream h t) = h : streamToList t

streamRepeat :: a -> Stream a
streamRepeat e = Stream e $ streamRepeat e

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream h t) = Stream (f h) $ streamMap f t

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Stream e $ streamFromSeed f (f e)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler =
-- interleaveStreams (streamRepeat 0) $
-- interleaveStreams (streamRepeat 1) . . .
ruler :: Stream Integer
ruler = f 0
  where f n = Stream n $ f (n + 1) `interleaveStreams` streamRepeat n

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream h1 t1) (Stream h2 t2) =
  Stream h1 $ Stream h2 $ interleaveStreams t1 t2

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger i = Stream i $ streamRepeat 0
  negate = streamMap negate
  (Stream h1 t1) + (Stream h2 t2) =
    Stream (h1 + h2) $ t1 + t2
  (Stream h1 t1) * b@(Stream h2 t2) =
    Stream (h1 * h2) $ streamMap (* h1) t2 + t1 * b
  abs = undefined
  signum = undefined

instance Fractional (Stream Integer) where
  (Stream h1 t1) / (Stream h2 t2) = q
    where q = Stream (div h1 h2) $
              streamMap (* div 1 h2) (t1 - q * t2)
  fromRational = undefined

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix a = Matrix a a a a
            deriving (Eq, Show)

instance Num a => Num (Matrix a) where
  Matrix a1 b1 c1 d1 * Matrix a2 b2 c2 d2 =
    Matrix (a1 * a2 + b1 * c2)
           (a1 * b2 + b1 * d2)
           (c1 * a2 + d1 * c2)
           (c1 * b2 + d1 * d2)
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

f :: Matrix Integer
f = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (\ (Matrix a _ _ _) -> a) $ f^n
