module Golf where

import Control.Applicative
import Data.List.Extra

skips :: [a] -> [[a]]
skips a =
  (\n -> chunksOf n a >>= drop (n - 1)) <$> [1..length a]
