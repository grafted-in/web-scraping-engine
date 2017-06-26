{-# LANGUAGE DeriveGeneric #-}

module ScrapingRules where

import qualified Data.Csv               as Csv
import qualified Data.Text              as T
import           ScrapeEngine.Prelude
import           Text.HTML.Scalpel.Core

import qualified ScrapeEngine as SE

data PageType = Sitemap | MemberListing
  deriving (Eq, Ord, Show)

type PageScraper a = AppState -> Url -> IO Text -> IO a

data BusinessData = BusinessData
  { catSearch       :: Maybe Text
  , nameSearch      :: Maybe Text
  , aboutSearch     :: Maybe Text
  , streetAddress   :: Maybe Text
  , addressLocality :: Maybe Text
  , addressRegion   :: Maybe Text
  , postalCode      :: Maybe Text
  , linkSearch      :: Maybe Text
  , phoneSearch1    :: Maybe Text
  , phoneSearch2    :: Maybe Text
  , faxSearch2      :: Maybe Text
  , repTitleSearch  :: Maybe Text
  , repNameSearch   :: Maybe Text
  , repPhone        :: Maybe Text
  }  deriving (Eq, Generic, Show)
instance Csv.DefaultOrdered BusinessData
instance Csv.ToNamedRecord BusinessData

seedPages :: [(PageType, Url)]
seedPages = [(Sitemap, Url "http://some-site.com/sitemap.xml")]

scrapePage :: PageType -> PageScraper [(PageType, Url)]
scrapePage pageType = case pageType of
  Sitemap       -> sitemapScraper
  MemberListing -> memberListingScraper

sitemapScraper :: PageScraper [(PageType, Url)]
sitemapScraper = scrapeForUrlsWith $
  chroots ("url" // "loc") $ do
    contents <- text anySelector
    guard ("/member/" `T.isInfixOf` contents)
    url <- text anySelector
    return (MemberListing, Url url)

memberListingScraper :: PageScraper [(PageType, Url)]
memberListingScraper = scrapeForRecordWith $ do
  catSearch        <- listToMaybe <$> texts ("ul" @: [hasClass "mn-member-cats"] // "li" )
  nameSearch       <- listToMaybe <$> texts ("div" @: [hasClass "mn-section-content"] // "div")
  aboutSearch      <- listToMaybe <$> texts ("div" @: [hasClass "mn-section-content"] // "p")

  streetAddress    <- listToMaybe <$> texts ("div" @: [hasClass "mn-address1"])
  addressLocality  <- listToMaybe <$> texts ("span" @: [hasClass "mn-cityspan"])
  addressRegion    <- listToMaybe <$> texts ("span" @: [hasClass "mn-stspan"])
  postalCode       <- listToMaybe <$> texts ("span" @: [hasClass "mn-zipspan"])

  linkSearch      <- listToMaybe <$> attrs "href" ("div" @: [hasClass "mn-memberinfo-block-actions"] // "ul" // "li" // "a")
  phoneSearch1    <- listToMaybe <$> texts ("div" @: [hasClass "mn-member-phone1"])
  phoneSearch2    <- listToMaybe <$> texts ("div" @: [hasClass "mn-member-phone2"])
  faxSearch2      <- listToMaybe <$> texts ("div" @: [hasClass "mn-member-fax"])

  repTitleSearch  <- listToMaybe <$> texts ("span" @: [hasClass "mn-rep-prefix"])
  repNameSearch   <- listToMaybe <$> texts ("span" @: [hasClass "mn-rep-fullname"])
  repPhone        <- listToMaybe <$> chroots ("div" @: [hasClass "mn-member-repphone"] // "span") (do
    contents <- text anySelector
    guard ("-" `T.isInfixOf` contents)
    text anySelector)

  return BusinessData{..}


newtype AppState = AppState { _insertRecord :: BusinessData -> IO () }

startScraper :: (BusinessData -> IO ()) -> SE.DownloadReqSink b -> IO ()
startScraper insertRecord sink =
  sink =<< forM seedPages (urlScraperReq (AppState insertRecord) sink)


scrapeForRecordWith :: Scraper Text BusinessData -> AppState -> Url -> IO Text -> IO [(PageType, Url)]
scrapeForRecordWith recordScraper AppState{_insertRecord} _ getPage = do
  contents <- getPage
  let eResult = scrapeStringLike contents recordScraper
  whenJust eResult _insertRecord
  pure []


scrapeForUrlsWith :: Scraper Text [(PageType, Url)] -> AppState -> Url -> IO Text -> IO [(PageType, Url)]
scrapeForUrlsWith scraper _ _ getPage = do
  contents <- getPage
  pure $ fromMaybe [] $ scrapeStringLike contents scraper


urlScraperReq :: AppState -> SE.DownloadReqSink b -> (PageType, Url) -> IO (SE.DownloadReq b)
urlScraperReq state sink (pageType, url_) = SE.urlScraperReq sink url_ (\getPage -> do
    newPages <- scrapePage pageType state url_ getPage
    forM newPages (urlScraperReq state sink)
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
