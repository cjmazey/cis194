module Main where

import Buffer.JoinList
import Buffer.JoinList.Sized
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "prop_indexJ" $
    \ x -> x + (1 :: Int) `seq` False
  ]
