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
mapA f = sequenceA . map f

sequenceA :: Applicative f
          => [f a] -> f [a]
sequenceA =
  foldr (liftA2 (:))
        (pure [])

replicateA :: Applicative f
           => Int -> f a -> f [a]
replicateA i f = sequenceA (replicate i f)
