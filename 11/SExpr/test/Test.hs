module Main where

import           AParser
import           Data.Char
import           SExpr

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
            [testCase "(no unit tests)" $
             True @?= True
            ,testCase "zeroOrMore 1" $
             runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" @?=
             Just ("ABC","dEfgH")
            ,testCase "oneOrMore 1" $
             runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" @?=
             Just ("ABC","dEfgH")
            ,testCase "zeroOrMore 2" $
             runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" @?=
             Just ("","abcdeFGh")
            ,testCase "oneOrMore 2" $
             runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" @?=
             Nothing
            ,testCase "ident 1" $
             runParser ident "foobar baz" @?=
             Just ("foobar"," baz")
            ,testCase "ident 2" $
             runParser ident "foo33fA" @?=
             Just ("foo33fA","")
            ,testCase "ident 3" $
             runParser ident "2bad" @?=
             Nothing
            ,testCase "ident 4" $
             runParser ident "" @?=
             Nothing]
