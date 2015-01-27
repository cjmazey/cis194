module HanoiG where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Maybe    (listToMaybe, mapMaybe)

type Peg = String
type Move = (Peg,Peg)

hanoiG :: Integer -> [Peg] -> Maybe [Move]
hanoiG 1 (s:d:_) = Just [(s,d)]
hanoiG n (s:d:p:ps) =
  listToMaybe $ sortBy (compare `on` length) mss
  where
    mss :: [[Move]]
    mss = mapMaybe ms [1..n-1]
    ms :: Integer -> Maybe [Move]
    ms k = do m   <- hanoiG k (s:p:d:ps)
              m'  <- hanoiG (n-k) (s:d:ps)
              m'' <- hanoiG k (p:d:s:ps)
              return $ m ++ m' ++ m''
hanoiG _ _ = Nothing
