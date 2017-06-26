module ScrapeEngine.FetchUrl where

import qualified Data.ByteString.Lazy as Bz
import           Data.Default         (def)
import           ScrapeEngine.Prelude

import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.Connection        (ProxySettings(SockSettingsSimple))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (mkManagerSettings, tlsManagerSettings)
import           Network.HTTP.Types.Header (hContentType, hUserAgent)
import           Network.Socks5.Types      (SocksError)


data FetchError = DontRetry | DoRetry deriving (Eq, Show)


mkHttpManagerProxied :: Text -> Int -> IO Manager
mkHttpManagerProxied proxyHost_ proxyPort_ =
  newManager $
    mkManagerSettings def (Just $ SockSettingsSimple (T.unpack proxyHost_) (fromIntegral proxyPort_))

mkHttpManager :: IO Manager
mkHttpManager = newManager tlsManagerSettings


getWith :: Manager -> Text -> Url -> IO (Either FetchError Text)
getWith manager userAgent (Url url) = do
  reqRaw <- parseUrlThrow (T.unpack url)
  let req = reqRaw{ requestHeaders = [(hUserAgent, encodeUtf8 userAgent)] }

  (Right .defaultDecoder <$> httpLbs req manager) `catches`
    [ Handler $ \(e :: HttpException) -> do
        say $ "Download failed for URL '" <> url <> "': " <> T.pack (show e)
        pure $ Left DontRetry
    , Handler $ \(e :: SocksError) -> do
        say $ "PROXY ERROR -> '" <> T.pack (show e)
        pure $ Left DoRetry
    ]


-- | The default response decoder. This decoder attempts to infer the character
-- set of the HTTP response body from the `Content-Type` header. If this header
-- is not present, then the character set is assumed to be `ISO-8859-1`.
--defaultDecoder :: CurlResponse -> Text
defaultDecoder :: Response Bz.ByteString -> Text
defaultDecoder response = choosenDecoder (Bz.toStrict $ responseBody response)
  where
    contentType = [ T.decodeUtf8 x
                  | (header, x) <- responseHeaders response
                  , header == hContentType
                  ]

    isType :: Text -> Bool
    isType t
      | [ct] <- contentType = T.toLower ("charset=" <> t) `T.isInfixOf` T.toLower ct
      | otherwise           = False

    choosenDecoder | isType "utf-8" = T.decodeUtf8
                   | otherwise      = T.decodeLatin1
