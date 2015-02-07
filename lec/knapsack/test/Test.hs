module Main where

import           Knapsack

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

sameCardinality :: Gen ([Double], [Integer])
sameCardinality =
  let ts :: Gen [(Positive Double, Positive Integer)]
      ts = arbitrary
  in do t <- ts
        let t' = do (Positive m, Positive n) <- t
                    return (m, n)
        return $ unzip t'

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "knapsack01 == knapsack01''" $
    \ (Positive w) ->
     forAll sameCardinality (\ (vs, ws) ->
                              classify (w == 1) "trivial (max weight)" $
                              classify (length vs <= 1) "trivial (length)" $
                              knapsack01 vs ws w == knapsack01'' vs ws w)
  ]
