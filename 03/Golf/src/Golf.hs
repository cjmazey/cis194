module Golf where

import Control.Applicative
import Data.List.Extra

skips :: [a] -> [[a]]
skips a =
  map foo [1..(length a)] <*> [a]
  where foo n = concatMap (drop (n-1)) . chunksOf n
