module Main where

import CreditCard

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testToDigits
    , testDoubleEveryOther
    , testSumDigits
    , testValidate
    ]

testToDigits :: TestTree
testToDigits =
  testGroup
    "toDigits"
    [ testCase "ex1" $ toDigits 1234 @=? [1,2,3,4]
    , testCase "ex2" $ toDigitsRev 1234 @=? [4,3,2,1]
    , testCase "ex3" $ toDigits 0 @=? []
    , testCase "ex4" $ toDigits (-17) @=? []
    ]

testDoubleEveryOther :: TestTree
testDoubleEveryOther =
  testGroup
    "doubleEveryOther"
    [ testCase "ex1" $ doubleEveryOther [8,7,6,5] @=? [16,7,12,5]
    , testCase "ex2" $ doubleEveryOther [1,2,3] @=? [1,4,3]
    ]

testSumDigits :: TestTree
testSumDigits =
  testGroup
    "sumDigits"
    [ testCase "ex1" $ sumDigits [16,7,12,5] @=? 1 + 6 + 7 + 1 + 2 + 5
    ]

testValidate :: TestTree
testValidate =
  testGroup
    "validate"
    [ testCase "ex1" $ validate 4012888888881881 @=? True
    , testCase "ex2" $ validate 4012888888881882 @=? False
    ]

