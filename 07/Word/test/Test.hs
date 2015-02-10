{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Buffer.JoinList
import           Buffer.JoinList.Scrabble
import           Buffer.JoinList.Sized
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

-- orphan instance
instance Arbitrary a => Arbitrary (JoinList Size a) where
  arbitrary =
    frequency
      [ (1, return Empty)
      , (2, liftM (Single (Size 1)) arbitrary)
      , (3, liftM2 (+++) arbitrary arbitrary)
      ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "prop_indexJ" $
    \ i l ->
     classify (length (jlToList l) < 5) "sort of trivial" $
     prop_indexJ i l
  , testProperty "prop_dropJ" $
    \ n l ->
     classify (length (jlToList l) < 5) "sort of trivial" $
     prop_dropJ n l
  , testProperty "prop_takeJ" $
    \ n l ->
     classify (length (jlToList l) < 5) "sort of trivial" $
     prop_takeJ n l
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "scoreLine" $
    scoreLine "yay " +++ scoreLine "haskell!" @?=
    Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
  ]
