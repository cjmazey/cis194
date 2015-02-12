module Main where

import           Party
import           Paths_Party



main :: IO ()
main = getDataFileName "company.txt" >>=
       readFile >>=
       putStrLn . formatGuestList . maxFun . read
