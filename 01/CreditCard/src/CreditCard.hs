module CreditCard where

import Data.Char ( digitToInt )

toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show i)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) xs ys
  where ys = if even $ length xs
             then cycle [2,1]
             else cycle [1,2]

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits

