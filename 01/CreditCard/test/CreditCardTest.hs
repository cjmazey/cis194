module Main where

import CreditCard

import Test.HUnit

testToDigits :: Test
testToDigits =
  test [ "ex1" ~: toDigits 1234 ~=? [1,2,3,4]
       , "ex2" ~: toDigitsRev 1234 ~=? [4,3,2,1]
       , "ex3" ~: toDigits 0 ~=? [3]
       , "ex4" ~: toDigits (-17) ~=? []
       ]

tests :: Test
tests =
  test [ "toDigits" ~: testToDigits
       ]

main = runTestTT tests
