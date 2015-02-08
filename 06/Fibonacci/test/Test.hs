module Main where

import           Fibonacci

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

rulerF :: Int -> Int
rulerF n | rem n 2 == 0 = 1 + rulerF (div n 2)
         | otherwise = 0

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "ruler == rulerF" $
    \ (Positive n) ->
     classify (n < 10) "n small" $
     streamToList ruler !! fromIntegral (n - 1) == fromIntegral (rulerF n)
  ]
