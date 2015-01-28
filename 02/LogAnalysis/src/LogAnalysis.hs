{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (parseMessage) where

import           Control.Applicative
import           Log
import           Paths_LogAnalysis   (getDataFileName)
import           Text.Parsec         (Parsec, anyChar, char, digit, eof, many1,
                                      parse, space, try)

ws :: Parsec String () ()
ws = () <$ many1 space

int :: Parsec String () Int
int = read <$> many1 digit

str :: Parsec String () String
str = many anyChar

messageType :: Parsec String () MessageType
messageType =
  Info <$ char 'I' <|>
  Warning <$ char 'W' <|>
  Error <$> (char 'E' *> ws *> int)

logMessage :: Parsec String () LogMessage
logMessage =
  try (LogMessage <$> messageType <* ws <*> int <* (ws <|> eof) <*> str) <|>
  Unknown <$> str

parseMessage :: String -> LogMessage
parseMessage s =
  case parse logMessage "" s of
    Right m -> m
    Left _ -> error "parseMessage"
