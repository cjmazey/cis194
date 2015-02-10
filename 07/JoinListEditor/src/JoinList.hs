module JoinList where

import           Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = Append

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append j k) = tag j <> tag k
