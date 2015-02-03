{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' =
  sum . takeWhile (/= 0) . filter even . iterate f
  where f 1 = 0
        f n
          | even n = n `div` 2
          | otherwise = 3 * n + 1
