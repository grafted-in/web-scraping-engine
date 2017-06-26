{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScrapeEngine.Prelude (
    module X,
    Url(..),
    ffor,
    readTextFile,
    seconds
  ) where

import Control.Applicative      as X
import Control.Concurrent       as X (threadDelay)
import Control.Concurrent.Async as X
import Control.Concurrent.MVar  as X (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Concurrent.STM   as X (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception        as X
import Control.Monad.Extra      as X
import Data.ByteString          as X (ByteString)
import Data.Csv                 (ToField)
import Data.Foldable            as X (toList)
import Data.Function            as X
import Data.List.NonEmpty       as X (NonEmpty, nonEmpty)
import Data.Maybe               as X
import Data.Monoid              as X
import Data.Text                as X (Text)
import Data.Text.Encoding       as X (decodeUtf8, encodeUtf8)
import GHC.Generics             as X (Generic)
import Path                     as X
import Say                      as X

import qualified Data.Text.IO as T

newtype Url = Url { urlAsText :: Text } deriving (Eq, Ord, ToField)
instance Show Url where
  show (Url url) = show url

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap


readTextFile :: Path b File -> IO Text
readTextFile = T.readFile . toFilePath

seconds :: Int -> Int
seconds = (*1000000)
