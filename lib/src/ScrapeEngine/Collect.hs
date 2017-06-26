{-# LANGUAGE DeriveGeneric #-}

module ScrapeEngine.Collect where

import qualified Control.Concurrent.Chan.Unagi as Chan
import qualified Data.ByteString.Base64.URL    as B64
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Data.IORef                    (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Path.IO                       (ensureDir, listDir)
import           ScrapeEngine.Prelude
import           System.Random.Shuffle         (shuffleM)

import ScrapeEngine.FetchUrl (FetchError(..), getWith, mkHttpManager, mkHttpManagerProxied)


data DownloadReq b = DownloadReq{
    _url         :: !Url,
    _signal      :: Maybe (Path b File) -> IO (),
    _ignoreCache :: !Bool
  } deriving Generic


data CollectorConfig b = CollectorConfig{
    _dataDir     :: !(Path b Dir),
    _curlProxies :: ![(Text, Int)],
    _userAgents  :: !(NonEmpty Text),
    _cacheOnly   :: !(Maybe CacheOnly),
    _ignoreCache :: !(Maybe IgnoreCache),
    _throttle    :: IO () -- a delaying action that throttles scraping
  } deriving Generic

data IgnoreCache = IgnoreCache deriving (Eq, Show)
data CacheOnly   = CacheOnly   deriving (Eq, Show)

type DownloadReqSink b = [DownloadReq b] -> IO ()


simpleDownloadReq :: Url -> DownloadReq b
simpleDownloadReq url = DownloadReq{
    _url         = url,
    _signal      = const (pure ()),
    _ignoreCache = False
  }


callbackDownloadReq :: Url -> (Maybe (Path b File) -> IO ()) -> IO (DownloadReq b)
callbackDownloadReq url callback = pure DownloadReq{
    _url         = url,
    _signal      = void . async . callback,
    _ignoreCache = False
  }


urlScraperReq :: DownloadReqSink b -> Url -> (IO Text -> IO [DownloadReq b]) -> IO (DownloadReq b)
urlScraperReq sink url scraper =
  callbackDownloadReq url $ \case
    Nothing -> sayErr $ "Unable to download " <> urlAsText url
    Just file ->
      sink =<< shuffleM =<< scraper (T.readFile (toFilePath file))


setUpCollectorChan :: CollectorConfig b -> IO (DownloadReqSink b, IO (DownloadReq b), Async ())
setUpCollectorChan cfg@CollectorConfig{_dataDir} = do
  ensureDir _dataDir
  cache <- Set.fromList . fmap fileToUrlKey . snd <$> listDir _dataDir
  say $ "Cache has " <> T.pack (show $ Set.size cache) <> " entries"
  visitedSet <- newIORef Set.empty

  (sink, source) <- Chan.newChan

  heartBeat <- async $ forever $ do
    size <- Set.size <$> readIORef visitedSet
    say $ ">>> Visited " <> T.pack (show size) <> " URLs"
    threadDelay $ seconds 1

  pure ( visitDownloadReqs cfg visitedSet cache (Chan.writeList2Chan sink)
       , Chan.readChan source
       , heartBeat
       )

-- ^ Starts collector threads for downloading pages.
runCollectors :: CollectorConfig b -> IO (DownloadReq b) -> IO ()
runCollectors
  CollectorConfig{
    _curlProxies, _dataDir, _userAgents, _throttle
  }
  readChan
  = do
  let proxyAgentZip = zip _curlProxies (cycle $ toList _userAgents)

  labeledManagers <- case proxyAgentZip of
    -- If there are no proxies, just use a normal HTTP connection
    [] -> do
      manager <- mkHttpManager
      pure [("", manager, NonEmpty.head _userAgents)]

    -- If there are proxies, create a separate connection for each
    _  -> forConcurrently proxyAgentZip $ \((proxyHost, proxyPort), userAgent) -> do
      manager <- mkHttpManagerProxied proxyHost proxyPort
      pure (proxyHost <> ":" <> T.pack (show proxyPort), manager, userAgent)

  forConcurrently_ labeledManagers $ \(label, manager, userAgent) -> do
    let
      logger msg = say $ label <> " | " <> msg
      getUrl url = _throttle >> getWith manager userAgent url

      go = forever $ do
        DownloadReq{_signal, _url} <- readChan
        logger $ "Visiting URL: " <> urlAsText _url
        let file = _dataDir </> urlKeyToFile (urlToKey _url)
        maybeError <- collectUrl logger getUrl file _url
        case maybeError of
          Just DontRetry -> _signal Nothing
          Just DoRetry   -> logger $ "SKIPPING " <> urlAsText _url
          Nothing        -> _signal (Just file)

    maybeIp <- getUrl (Url "https://ipinfo.io/ip")
    case maybeIp of
      Left _   -> logger ">>> Failed to find IP of exit node"
      Right ip -> do
        logger $ "IP: " <> T.strip ip <> " | User Agent: " <> userAgent
        go


getUnvisitedMap :: IORef (Set.HashSet ByteString) -> Map.HashMap ByteString a -> IO (Map.HashMap ByteString a)
getUnvisitedMap visitedSetRef newItems = atomicModifyIORef' visitedSetRef $ \visitedSet ->
  let
    unvisitedItems = newItems `Map.difference` Set.toMap visitedSet
    newVisitedSet = visitedSet `Set.union` Set.fromMap (Map.map (const ()) unvisitedItems)
  in (newVisitedSet, unvisitedItems)


visitDownloadReqs :: CollectorConfig b
                  -> IORef (Set.HashSet ByteString)
                  -> Set.HashSet ByteString
                  -> DownloadReqSink b
                  -> [DownloadReq b]
                  -> IO ()
visitDownloadReqs CollectorConfig{_dataDir, _ignoreCache, _cacheOnly} visitedSetRef cacheSet reqSink reqs = do
  let reqMap = Map.fromList [ (urlToKey _url, req) | req@DownloadReq{_url} <- reqs ]
  unvisited <- getUnvisitedMap visitedSetRef reqMap

  let
    -- If the "Ignore Cache" setting is enabled, visit all unvisited pages.
    -- Otherwise, visit only the pages that are dynamically set to ignore the cache.
    unvisitedAndIgnoreCache = if _ignoreCache == Just IgnoreCache
      then unvisited
      else Map.filter (\DownloadReq{_ignoreCache} -> not _ignoreCache) unvisited
    -- Determine which pages need to be downloaded but are already cached.
    cached = unvisitedAndIgnoreCache `Map.intersection` Set.toMap cacheSet
    -- Remove these pages from the total list of unvisited pages and download those.
    needDownload = unvisited `Map.difference` cached

    -- Define a sink based on our configuration:
    --   If we can only use the cache, skip all download requests.
    --   Otherwise, use the real sink.
    sink xs = if _cacheOnly == Just CacheOnly
      then forM_ xs $ \DownloadReq{_url} -> say $ "Cache only: skipping " <> urlAsText _url
      else reqSink xs

  sink $ Map.elems needDownload
  forM_ (Map.toList cached) $ \(key, req) ->
    _signal req (Just $ _dataDir </> urlKeyToFile key)


collectUrl :: (Text -> IO ())
           -> (Url -> IO (Either FetchError Text))
           -> Path b File
           -> Url
           -> IO (Maybe FetchError)
collectUrl loggerRaw getUrl file url = do
  let logger msg = loggerRaw (urlAsText url <> " | " <> msg)

  logger $ "Downloading to " <> T.pack (toFilePath file)
  eResp <- getUrl url
  case eResp of
    Left e     -> logger "DOWNLOAD FAILED" >> pure (Just e)
    Right resp -> do
        T.writeFile (toFilePath file) resp
        logger "Done"
        pure Nothing
      `catch` \(e :: SomeException) -> do
        logger $ "FAILED TO WRITE FILE: " <> T.pack (show e)
        pure (Just DontRetry)

urlToKey :: Url -> ByteString
urlToKey = B64.encode . encodeUtf8 . T.toLower . urlAsText

urlKeyToFile :: ByteString -> Path Rel File
urlKeyToFile = fromJust . parseRelFile . T.unpack . decodeUtf8

urlFromKey :: ByteString -> Url
urlFromKey = Url . decodeUtf8 . B64.decodeLenient

fileToUrlKey :: Path b File -> ByteString
fileToUrlKey = encodeUtf8 . T.pack . toFilePath . filename
