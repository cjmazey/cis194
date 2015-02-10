{-# LANGUAGE FlexibleInstances #-}

module Buffer.JoinList where

import           Buffer
import           Buffer.JoinList.Scrabble
import           Buffer.JoinList.Sized
import           Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ k = k
j +++ Empty = j
j +++ k = Append (tag j <> tag k) j k

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ j k)
  | i < s = indexJ i j
  | otherwise = indexJ (i - s) k
  where s = getSize $ size $ tag j

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n s@(Single _ _)
  | n > 0 = Empty
  | otherwise = s
dropJ n (Append _ l1 l2)
  | n <= s = dropJ n l1 +++ l2
  | otherwise = dropJ (n - s) l2
  where s = getSize $ size $ tag l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n s@(Single _ _)
  | n < 1 = Empty
  | otherwise = s
takeJ n (Append _ l1 l2)
  | n <= s = takeJ n l1
  | otherwise = l1 +++ takeJ (n - s) l2
  where s = getSize $ size $ tag l1

splitAtJ :: (Sized b, Monoid b) =>
          Int ->
          JoinList b a ->
          (JoinList b a, JoinList b a)
splitAtJ n jl = (takeJ n jl, dropJ n jl)

foldJ :: z -> (m -> a -> z) -> (m -> z -> z -> z) -> JoinList m a -> z
foldJ z _ _ Empty = z
foldJ _ f _ (Single m a) = f m a
foldJ z f g (Append m jl1 jl2) = g m (foldJ z f g jl1) (foldJ z f g jl2)

scoreLine :: String -> JoinList Score String
scoreLine = scoreString >>= Single

type DJoinList = JoinList (Score, Size) String

instance Buffer DJoinList  where
  toString = unlines . jlToList
  fromString s = fromLines $ lines s
    where fromLines [] = Empty
          fromLines [l] =
            Single (scoreString l, 1) l
          fromLines ls = fromLines ls' +++ fromLines ls''
            where (ls', ls'') = splitAt (length ls `div` 2) ls
  line = indexJ
  replaceLine i l jl = replaceLine' (splitAtJ i jl)
    where replaceLine' (pre, Empty) = pre
          replaceLine' (pre, suf) = pre +++ fromString l +++ dropJ 1 suf
  numLines = getSize . snd . tag
  value = getScore . fst . tag

-- tests

(!!?) :: [a] -> Int -> Maybe a
[]       !!? _          = Nothing
_        !!? i | i <  0 = Nothing
(x : _)  !!? 0          = Just x
(_ : xs) !!? i          = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

prop_indexJ :: Int -> JoinList Size Char -> Bool
prop_indexJ i jl = indexJ i jl == jlToList jl !!? i

prop_dropJ :: Int -> JoinList Size Char -> Bool
prop_dropJ n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

prop_takeJ :: Int -> JoinList Size Char -> Bool
prop_takeJ n jl = jlToList (takeJ n jl) == take n (jlToList jl)
