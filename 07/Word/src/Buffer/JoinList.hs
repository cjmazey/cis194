module Buffer.JoinList where

import           Buffer.JoinList.Scrabble
import           Buffer.JoinList.Sized
import           Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
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

scoreLine :: String -> JoinList Score String
scoreLine = mconcat . map scoreString . words >>= Single

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
