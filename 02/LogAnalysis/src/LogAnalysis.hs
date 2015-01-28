{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (parseMessage, parseLog, insert, build, inOrder, whatWentWrong, getDataFileName) where

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

parseLog :: String -> [LogMessage]
parseLog s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert l@(LogMessage _ _ _) Leaf =
  Node Leaf l Leaf
insert l@(LogMessage _ t _) (Node ml l'@(LogMessage _ t' _) mr)
  | t <= t' = Node (insert l ml) l' mr
  | otherwise = Node ml l' (insert l mr)
insert _ (Node _ (Unknown _) _) = error "unreachable"

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ml l mr) = inOrder ml ++ [l] ++ inOrder mr

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  (map msg) . inOrder . build . (filter f)
    where
      msg (LogMessage _ _ s) = s
      msg (Unknown _) = error "unreachable"
      f (LogMessage (Error e) _ _)
        | e >= 50 = True
      f _ = False
