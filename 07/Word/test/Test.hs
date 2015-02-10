module Main where

import Buffer.JoinList
import Buffer.JoinList.Sized
import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck

-- orphan instance
instance (Arbitrary Size) where
  arbitrary =
    let x :: Gen (NonNegative Int)
        x = arbitrary
    in do (NonNegative x') <- x
          return $ Size x'

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "prop_indexJ" $
    \ x -> x + (1 :: Int) `seq` False
  ]
