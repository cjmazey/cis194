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

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = (-1)
height (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node _ l y r)
  | hl < hr = Node (max hl' hr + 1) l' y r
  | otherwise = Node (max hl hr' + 1) l y r'
  where hl = height l
        hr = height r
        l'@(Node hl' _ _ _) = insertTree x l
        r'@(Node hr' _ _ _) = insertTree x r

prop_Height :: Tree Char -> Bool
prop_Height t =
  height t == height' t
  where height' Leaf = (-1)
        height' (Node _ l _ r) =
          1 + max (height' l) (height' r)

prop_Balanced :: Tree Char -> Bool
prop_Balanced Leaf = True
prop_Balanced (Node _ l _ r) =
  abs (height l - height r) <= 1 &&
  prop_Balanced l &&
  prop_Balanced r

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e a -> f e : a) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g z -> g (f z x)) id xs base

-- Exercise 4
