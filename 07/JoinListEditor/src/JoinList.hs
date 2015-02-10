module JoinList where

data JoinList m a = Empty
                  | Single m a
                  | Append (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)
