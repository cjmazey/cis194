{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Paths_LogAnalysis (getDataFileName)
import qualified Text.Parsec as P

parseMessageType :: P.Parsec String () MessageType
parseMessageType = info P.<|> warning P.<|> error
  where
    info    = P.char 'I' >> return Info
    warning = P.char 'W' >> return Warning
    error   = do P.char 'E'
                 P.many1 P.space
                 digits <- P.many1 P.digit
                 return $ Error (read digits)

parseLogMessage :: P.Parsec String () LogMessage
parseLogMessage = P.try logMessage P.<|> unknown
  where
    logMessage = do messageType <- parseMessageType
                    P.many1 P.space
                    timeStamp <- do digits <- P.many1 P.digit
                                    return (read digits)
                    (P.many1 P.space >> return ()) P.<|> P.eof
                    string <- P.many P.anyChar
                    return $ LogMessage messageType timeStamp string
    unknown = do string <- P.many P.anyChar
                 return $ Unknown string

parseMessage :: String -> LogMessage
parseMessage s =
  case P.parse parseLogMessage "" s of
    Right m -> m
    Left _ -> error "parseMessage"

