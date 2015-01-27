module Main where

import           HanoiG

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testHanoi]

testHanoi :: TestTree
testHanoi =
  testGroup "hanoiG"
            [testCase "ex1" $
             hanoiG 2 ["a", "b", "c"] @=?
             Just [("a","c"),("a","b"),("c","b")],
             testCase "length" $
             fmap length (hanoiG 15 ["a", "b", "c", "d"]) @=?
             Just 129]
