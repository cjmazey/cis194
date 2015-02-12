module Main where

import           Party
import           Paths_Party

main :: IO ()
main =
  let process :: String -> String
      process = formatGuestList . maxFun . read
  in getDataFileName "company.txt" >>=
     readFile >>=
     putStrLn . process
