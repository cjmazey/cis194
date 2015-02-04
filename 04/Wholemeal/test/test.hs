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
  , QC.testProperty "fun2 == fun2'" $
    \(Positive x) ->
     --QC.collect x $
     QC.classify (x == 1) "trivial" $
     fun2 x == fun2' x
  , QC.testProperty "prop_Height" $
    \xs ->
    QC.classify (length xs < 2) "trivial" $
    QC.classify (length xs > 10) "large" $
    prop_Height $ foldTree xs
  , QC.testProperty "prop_Balanced" $
    \xs ->
    QC.classify (length xs < 2) "trivial" $
    QC.classify (length xs > 10) "large" $
    prop_Balanced $ foldTree xs
  , QC.testProperty "xor" $
    \xs ->
    QC.classify (length xs < 2) "trivial" $
    xor xs == (odd . length . filter id) xs
  ]
