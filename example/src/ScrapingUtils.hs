{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}

module ScrapingUtils where

import qualified Data.Csv               as Csv
import qualified Data.Text              as T
import qualified ScrapeEngine           as SE
import           ScrapeEngine.Prelude
import           Text.HTML.Scalpel.Core (AttributePredicate, Scraper, Selector, TagName(..), match, scrapeStringLike,
                                        (@:), (@=))

newtype AppState = AppState { _insertRecord :: forall r. IsScrapeRecord r => r -> IO () }

class IsPageType t where
  toScraper :: t -> PageScraper [(t, Url)]

type IsScrapeRecord r = (Show r, Csv.DefaultOrdered r, Csv.ToNamedRecord r)

type PageScraper a = AppState -> Url -> IO Text -> IO a

startScraper :: IsPageType t
             => [(t, Url)] -> (forall r. IsScrapeRecord r => r -> IO ()) -> SE.DownloadReqSink b -> IO ()
startScraper seedPages insertRecord sink =
  sink =<< forM seedPages (urlScraperReq (AppState insertRecord) sink)


scrapeForRecordWith :: forall r t. IsScrapeRecord r
                    => Scraper Text r -> AppState -> Url -> IO Text -> IO [(t, Url)]
scrapeForRecordWith recordScraper AppState{_insertRecord} _ getPage = do
  contents <- getPage
  let eResult :: Maybe r = scrapeStringLike contents recordScraper
  whenJust eResult _insertRecord
  pure []


scrapeForUrlsWith :: IsPageType t
                  => Scraper Text [(t, Url)] -> AppState -> Url -> IO Text -> IO [(t, Url)]
scrapeForUrlsWith scraper _ _ getPage = do
  contents <- getPage
  pure $ fromMaybe [] $ scrapeStringLike contents scraper


urlScraperReq :: IsPageType t
              => AppState -> SE.DownloadReqSink b -> (t, Url) -> IO (SE.DownloadReq b)
urlScraperReq appState sink (pageType, url_) = SE.urlScraperReq sink url_ (\getPage -> do
    newPages <- toScraper pageType appState url_ getPage
    forM newPages (urlScraperReq appState sink)
  )


absUrl :: Url -> Url -> Url
absUrl (Url base) r@(Url other)
  | "http://" `T.isPrefixOf` otherNorm || "https://" `T.isPrefixOf` otherNorm = r
  | otherwise = Url (T.dropWhileEnd (=='/') base <> "/" <> T.dropWhile (=='/') other)
  where
    otherNorm = T.toLower other


atId :: String -> Selector
atId id_ = AnyTag @: ["id" @= id_]

attrSatisfies :: String -> (String -> Bool) -> AttributePredicate
attrSatisfies attrName predicate = match
  (\givenAttr val -> (givenAttr == attrName) && predicate val)
