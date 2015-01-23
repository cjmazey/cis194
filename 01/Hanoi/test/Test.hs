module Main where

import Hanoi

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testHanoi]

testHanoi :: TestTree
testHanoi = 
  testGroup "hanoi"
            [testCase "ex1" $
             hanoi 2 "a" "b" "c" @=?
             [("a","c"),("a","b"),("c","b")]]
