module ScrapeEngine.TorProxies where

import qualified Data.Text as T
import qualified Data.Text.Read as T


-- ^ Parses a torrc file and extracts all SOCKSPorts entries.
parseTorRcForSocksPorts :: T.Text -> [(T.Text, Int)]
parseTorRcForSocksPorts txt =
  [ (host, port)
  | Just socksPort <- extractSocksPort . T.words . T.stripStart <$> T.lines txt
  , [host, portTxt] <- [T.splitOn ":" socksPort]
  , Right (port, _) <- [T.decimal portTxt]
  ]

extractSocksPort :: [T.Text] -> Maybe T.Text
extractSocksPort (first:second:_)
  | T.toLower first == T.toLower "SOCKSPort" = Just second
  | otherwise = Nothing
extractSocksPort _ = Nothing
