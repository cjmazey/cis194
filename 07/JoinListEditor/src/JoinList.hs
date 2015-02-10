module JoinList where

import           Data.Monoid
import Sized

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
indexJ i (Append _ j k) | i < s = indexJ i j
                        | otherwise = indexJ (i - s) k
  where s = getSize $ size $ tag j

(!!?) :: [a] -> Int -> Maybe a
[]       !!? _          = Nothing
_        !!? i | i <  0 = Nothing
(x : _)  !!? 0          = Just x
(_ : xs) !!? i          = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

prop_indexJ ::(Sized b, Monoid b, Eq a) => Int -> JoinList b a -> Bool
prop_indexJ i jl = indexJ i jl == jlToList jl !!? i
