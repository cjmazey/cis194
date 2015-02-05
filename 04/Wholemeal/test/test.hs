module Main where

import           Wholemeal

import           Test.QuickCheck.Function
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ testProperty "fun1 == fun1'" $
    \xs ->
     classify (length xs < 2) "trivial" $
     classify (2 `elem` xs) "contains 2" $
     fun1 xs == fun1' xs
  , testProperty "fun2 == fun2'" $
    \(Positive x) ->
     classify (x == 1) "trivial" $
     fun2 x == fun2' x
  , testProperty "prop_Height" $
    \xs ->
    classify (length xs < 2) "trivial" $
    classify (length xs > 10) "large" $
    prop_Height $ foldTree xs
  , testProperty "prop_Balanced" $
    \xs ->
    classify (length xs < 2) "trivial" $
    classify (length xs > 10) "large" $
    prop_Balanced $ foldTree xs
  , testProperty "xor" $
    \xs ->
    classify (length xs < 2) "trivial" $
    xor xs == (odd . length . filter id) xs
  , testProperty "map' = map" $
    let p :: (Fun Integer Integer) -> [Integer] -> Property
        p f xs =
          classify (length xs < 2) "trivial" $
          map' (apply f) xs == map (apply f) xs
    in p
  , testProperty "myFoldl = foldl" $
    let p :: (Fun Integer (Fun Integer Integer)) -> Integer -> [Integer] -> Property
        p f z xs =
          classify (length xs < 2) "trivial" $
          myFoldl f' z xs == foldl f' z xs
          where f' = apply . (apply f)
    in p
  , testProperty "sieveSundaram = sieveEratosthenes" $
    \(Positive n) ->
    classify (n < 2) "trivial" $
    2 : sieveSundaram n == sieveEratosthenes (2 * n + 2)
  ]
