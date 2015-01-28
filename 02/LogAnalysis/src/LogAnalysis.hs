{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Paths_LogAnalysis (getDataFileName)
import qualified Text.Parsec as P
import Text.Parsec (Parsec, char, many1, space, digit, try, eof, anyChar)
import Control.Applicative

whiteSpace :: Parsec String () ()
whiteSpace = () <$ many1 space

int :: Parsec String () Int
int = read <$> many1 digit

parseMessageType :: Parsec String () MessageType
parseMessageType =
  Info <$ char 'I' <|>
  Warning <$ char 'W' <|>
  Error <$> (char 'E' *> whiteSpace *> int)

parseLogMessage :: Parsec String () LogMessage
parseLogMessage =
  try (LogMessage <$> parseMessageType <* whiteSpace <*> int <* whiteSpace <*> many anyChar) <|>
  Unknown <$> many anyChar

parseMessage :: String -> LogMessage
parseMessage s =
  case P.parse parseLogMessage "" s of
    Right m -> m
    Left _ -> error "parseMessage"

