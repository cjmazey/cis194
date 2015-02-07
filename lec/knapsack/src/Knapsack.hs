module Knapsack where

import           Data.Array
import           Data.List

knapsack01 :: [Double]   -- values
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best)
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w))
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example :: Double
example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20


knapsack01' :: [Double]
            -> [Integer]
            -> Integer
            -> Array (Integer, Integer) Double
knapsack01' values weights maximumWeight =
  let n = genericLength values
      vs = listArray (1, n) values
      ws = listArray (1, n) weights
      mw = maximumWeight
      m = array ((0, 0), (n, mw))
                [((i, w), f (i, w)) | i <- [0..n], w <- [0..mw]]
      f (i, w) | i == 0 = 0
               | w == 0 = 0
               | ws!i > w = m!(i - 1, w)
               | otherwise = max (m!(i - 1, w))
                                 (m!(i - 1, w - ws!i) + vs!i)
  in m

knapsack01'' vs ws maxW =
  m!(genericLength vs, maxW)
  where m = knapsack01' vs ws maxW
