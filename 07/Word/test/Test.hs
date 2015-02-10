module Main where

import Buffer.JoinList
import Buffer.JoinList.Sized
import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck

-- orphan instance
instance (Arbitrary Size) where
  arbitrary = liftM (Size . getNonNegative) arbitrary

-- orphan instance
instance (Arbitrary m, Arbitrary a) => Arbitrary (JoinList m a) where
  arbitrary =
    frequency
      [ (1, return Empty)
      , (2, liftM2 Single arbitrary arbitrary)
      , (1, liftM3 Append arbitrary arbitrary arbitrary)
      ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "prop_indexJ" $
    \ i l ->
     classify (length (jlToList l) < 5) "sort of trivial" $
     prop_indexJ i l
  ]
