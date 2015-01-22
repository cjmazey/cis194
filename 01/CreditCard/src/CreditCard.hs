module CreditCard where

import Data.Char ( digitToInt )

toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show i)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x : x' : xs) =
  x : 2 * x' : doubleEveryOtherRev xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
