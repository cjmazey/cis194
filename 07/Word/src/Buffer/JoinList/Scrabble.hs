{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buffer.JoinList.Scrabble where

import qualified Data.Map.Strict as M
import           Data.Monoid

newtype Score = Score { getScore :: Int }
              deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = 0
  mappend = (+)

scores :: M.Map Char Score
scores = M.fromList $
         zip (['A' .. 'Z'] ++ ['a' .. 'z']) $
         cycle [1,3,3,2,1,4,2,4,1,8,5,1,3
               ,1,1,3,10,1,1,1,1,4,4,8,4,10
               ]

score :: Char -> Score
score c = M.findWithDefault 0 c scores

scoreString :: String -> Score
scoreString = mconcat . map score
