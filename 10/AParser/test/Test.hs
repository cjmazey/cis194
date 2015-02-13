module Main where

import           AParser

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

properties :: TestTree
properties =
  testGroup "Properties"
            [testProperty "(no properties)" $
             (const True :: Bool -> Bool)]

unitTests :: TestTree
unitTests =
  testGroup "Unit tests"
            [testCase "abParser 1" $
             runParser abParser "abcdef" @?=
             Just (('a','b'),"cdef")
            ,testCase "abParser 2" $
             runParser abParser "aebcdf" @?=
             Nothing
            ,testCase "abParser_ 1" $
             runParser abParser_ "abcdef" @?=
             Just ((),"cdef")
            ,testCase "abParser_ 2" $
             runParser abParser_ "aebcdf" @?=
             Nothing
            ,testCase "intPair" $
             runParser intPair "12 34" @?=
             Just ([12,34],"")
            ,testCase "intOrUppercase 1" $
             runParser intOrUppercase "342abcd" @?=
             Just ((),"abcd")
            ,testCase "intOrUppercase 2" $
             runParser intOrUppercase "XYZ" @?=
             Just ((),"YZ")
            ,testCase "intOrUppercase 3" $
             runParser intOrUppercase "foo" @?=
             Nothing]
