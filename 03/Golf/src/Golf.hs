module Golf where

import           Control.Applicative
import           Data.List.Extra

{-|
'<$>' is essentially 'fmap', which is 'map' for lists.
The mapped anonymous function maps @i@ to a list containing every @i@-th
element of @a@.

The anonymous function:

* breaks up @a@ into chunks of size @i@.
* drops from each chunk @i-1@ elements, thus giving a list of lists of
@i@-th elements of @a@.
* concatenates that list ('>>=' is 'concatMap' for lists.)
-}
skips :: [a] -> [[a]]
skips a =
  (\n -> chunksOf n a >>= drop (n - 1)) <$> [1..length a]

{-|
'zip3' gives triples consisting of all sub-sequences of length 3 in @a@.

The guards filter out the triples with local maximums in the middle.

Then we return a list of these middles.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima a =
  [y | (x,y,z) <- zip3 a (drop 1 a) (drop 2 a), x < y, y > z]
