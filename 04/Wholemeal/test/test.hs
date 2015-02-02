module Main where

import           Wholemeal

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fun1 == fun1'" $
      \xs ->
       QC.classify (length xs < 2) "trivial" $
       QC.classify (2 `elem` xs) "contains 2" $
       fun1 xs == fun1' xs
  ]
