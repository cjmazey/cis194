module Golf where

import Control.Applicative
import Data.List.Extra

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
-}
localMaxima :: [Integer] -> [Integer]
localMaxima (x:r@(y:z:_))
  | y > x && y > z = y : localMaxima r
  | otherwise = localMaxima r
localMaxima _ = []
