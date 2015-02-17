{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

------------------------------------------------------------
-- Die values

newtype DieValue =
  DV {unDV :: Int}
  deriving (Eq,Ord,Show,Num)

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Random DieValue where
  random =
    first DV .
    randomR (1,6)
  randomR (low,hi) =
    first DV .
    randomR (max 1 (unDV low),min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield =
  Battlefield {attackers :: Army
              ,defenders :: Army}
  deriving (Show)

battle' :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battle' b aDice dDice =
  uncurry Battlefield $
  foldl (\(a,d) (aDie,dDie) ->
           if aDie > dDie
              then (a,d - 1)
              else (a - 1,d))
        (attackers b,defenders b)
        (zip (sortBy (flip compare) aDice)
             (sortBy (flip compare) dDice))

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  do as <-
       replicateM (min 3 (attackers b - 1))
                  die
     ds <-
       replicateM (min 2 (defenders b))
                  die
     return $
       battle' b as ds

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | attackers b < 2 || defenders b == 0 = return b
  | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  do bs <-
       replicateM 1000
                  (invade b)
     return $
       fromIntegral
         (length $
          filter ((== 0) . defenders) bs) /
       1000
