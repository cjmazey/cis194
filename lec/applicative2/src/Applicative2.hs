module Applicative2 where

import           AParser
import           Control.Applicative hiding ((*>))

pair :: Applicative f
     => f a -> f b -> f (a,b)
pair = liftA2 (,)

(*>) :: Applicative f
     => f a -> f b -> f b
u *> v = const id <$> u <*> v

mapA :: Applicative f
     => (a -> f b) -> ([a] -> f [b])
mapA = undefined

sequenceA :: Applicative f
          => [f a] -> f [a]
sequenceA fs = undefined

replicateA :: Applicative f
           => Int -> f a -> f [a]
replicateA = undefined
