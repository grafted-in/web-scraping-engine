{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}

module ScrapeEngine.Main where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.Chan.Unagi as Chan
import qualified Data.Csv                      as Csv
import qualified Data.Text                     as T
import qualified Options.Applicative           as O
import           Path.IO                       (listDir)
import           System.Random                 (randomRIO)
import           System.Random.Shuffle         (shuffleM)

import           ScrapeEngine            (CacheOnly(..), IgnoreCache(..))
import qualified ScrapeEngine            as SE
import           ScrapeEngine.CsvOut     (writeCsv)
import           ScrapeEngine.Prelude
import qualified ScrapeEngine.TorProxies as SE

type IsScrapeRecord r = (Show r, Csv.DefaultOrdered r, Csv.ToNamedRecord r)
type RecordInserter = forall r. IsScrapeRecord r => r -> IO ()

type Scraper b = RecordInserter -> SE.DownloadReqSink b -> IO ()

parseArgs :: [String] -> IO Options
parseArgs args = O.handleParseResult $ O.execParserPure O.defaultPrefs opts args
  where
    opts = O.info (O.helper <*> cmdOpts) (O.fullDesc <> O.progDesc "Run scraper commands")


mainWithArgs :: Options -> Scraper Rel -> (Int -> IO ()) -> IO ()
mainWithArgs Options{_cacheDir, _optionsChoice} scraper startMonitor =
  case _optionsChoice of
    ListCacheOption -> listCache _cacheDir
    ScrapeOption ScrapingOptions{..} -> do
      userAgentsList <- shuffleM =<< T.lines <$> readTextFile _userAgentsFile
      userAgents <- case nonEmpty userAgentsList of
        Nothing -> throwIO $ userError "User agents file is empty"
        Just x  -> pure x

      torProxies <- case _torrcFile of
        Just file -> shuffleM =<< SE.parseTorRcForSocksPorts <$> readTextFile file
        Nothing   -> pure []

      let cfg = SE.CollectorConfig{
              _dataDir     = _cacheDir,
              _curlProxies = torProxies,
              _userAgents  = userAgents,
              _cacheOnly   = _cacheOnly,
              _ignoreCache = _ignoreCache,
              _throttle    = randomRIO (seconds _delayMin, seconds _delayMax) >>= threadDelay
            }

      case _monitorPort of
        Nothing   -> pure ()
        Just port -> startMonitor port

      runScraper _outputFile cfg scraper

data Item = forall r. IsScrapeRecord r => Item r

runScraper :: Path b File -> SE.CollectorConfig b -> Scraper b -> IO ()
runScraper outFile cfg scraper = do
  (sink, source, heartBeatThread) <- SE.setUpCollectorChan cfg
  collectorThread <- async $ SE.runCollectors cfg source >> cancel heartBeatThread

  (fileSink, fileSource) <- Chan.newChan
  writerThread <- async $ Chan.readChan fileSource >>= \(Item r) -> writeCsv outFile r

  Async.link2 collectorThread writerThread
  Async.link2 collectorThread heartBeatThread
  Async.link heartBeatThread
  Async.link collectorThread

  scraper (Chan.writeChan fileSink) sink
  wait collectorThread


listCache :: Path a Dir -> IO ()
listCache cacheDir = do
  (_, files) <- listDir cacheDir
  forM_ files $ \file ->
    say $ T.pack (toFilePath $ filename file) <> " -> " <> urlAsText (SE.urlFromKey $ SE.fileToUrlKey file)


data Options = Options{
    _cacheDir      :: Path Rel Dir,
    _optionsChoice :: OptionsChoice
  }

data OptionsChoice = ListCacheOption | ScrapeOption ScrapingOptions

data ScrapingOptions = ScrapingOptions{
    _ignoreCache    :: Maybe IgnoreCache,
    _cacheOnly      :: Maybe CacheOnly,
    _userAgentsFile :: Path Rel File,
    _outputFile     :: Path Rel File,
    _delayMin       :: Int,
    _delayMax       :: Int,
    _torrcFile      :: Maybe (Path Rel File),
    _monitorPort    :: Maybe Int
  }


cmdOpts :: O.Parser Options
cmdOpts = Options
    <$> cacheDirOpt
    <*> (listCacheOpts <|> (ScrapeOption <$> scrapingOpts))
  where
    cacheDirOpt = O.option (readWith parseRelDir)
      (  O.long "cache-dir"
      <> O.short 'c'
      <> O.metavar "DIR"
      <> O.help "Directory for storing cached pages"
      <> O.value [reldir|_cache|]
      <> O.showDefault
      )

    listCacheOpts = O.flag' ListCacheOption
      (  O.long "list-cache"
      <> O.help "List all URLs in the cache"
      )

    scrapingOpts = ScrapingOptions
      <$> ( unblind IgnoreCache <$> O.switch
            (  O.long "ignore-cache"
            <> O.help "Run scraper and don't use cached data"
            )
          )
      <*> ( unblind CacheOnly <$> O.switch
            (  O.long "cache-only"
            <> O.help "Run scraper and only scrape pages in the cache (implies --delay-max of 0)"
            )
          )
      <*> O.option (readWith parseRelFile)
            (  O.long "user-agents"
            <> O.short 'a'
            <> O.metavar "FILE"
            <> O.help "Name of file containing list of user agent headers"
            )
      <*> O.option (readWith parseRelFile)
            (  O.long "output-file"
            <> O.short 'o'
            <> O.metavar "FILE"
            <> O.help "Name of file to write CSV data"
            )
      <*> O.option O.auto
            (  O.long "delay-min"
            <> O.metavar "SECONDS"
            <> O.help "Minimum time to wait between throttled requests"
            <> O.value 0
            )
      <*> O.option O.auto
            (  O.long "delay-max"
            <> O.metavar "SECONDS"
            <> O.help "Maximum time to wait between throttled requests"
            <> O.value 5
            )
      <*> O.optional (O.option (readWith parseRelFile)
            (  O.long "torrc"
            <> O.metavar "FILE"
            <> O.help "Name of torrc to use for proxying"
            ))
      <*> O.optional (O.option O.auto
            (  O.long "monitor-port"
            <> O.short 'm'
            <> O.metavar "PORT"
            <> O.help "Run a web-based monitor over HTTP on PORT"
            ))

readWith :: Show err => (String -> Either err b) -> O.ReadM b
readWith f = O.eitherReader (either (Left . show) Right . f)


unblind :: a -> Bool -> Maybe a
unblind a cond = if cond then Just a else Nothing
