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
height Leaf = undefined
height (Node _ Leaf _ Leaf) = 0
height (Node _ l _ Leaf) = 1 + height l
height (Node _ Leaf _ r) = 1 + height r
height (Node _ l _ r) = 1 + max (height l) (height r)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a' (Node h l a r) =
  case (l, r) of
   (Leaf, Leaf) -> Node 1 Leaf a t
   (Leaf, _)    -> Node h t a r
   (_, Leaf)    -> Node h l a t
   (Node lh _ _ _, Node rh _ _ _)
     | lh < rh   -> Node (rh + 1) (insertTree a' l) a r
     | otherwise -> Node (lh + 1) l a (insertTree a' r)
  where t  = insertTree a' Leaf
