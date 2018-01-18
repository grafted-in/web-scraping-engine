{-# LANGUAGE DeriveGeneric #-}

module ScrapingRules where

import qualified Data.Csv               as Csv
import qualified Data.Text              as T
import           ScrapeEngine.Prelude
import           Text.HTML.Scalpel.Core

import ScrapingUtils


data PageType = Sitemap | MemberListing
  deriving (Eq, Ord, Show)

instance IsPageType PageType where
  toScraper pageType = case pageType of
    Sitemap       -> sitemapScraper
    MemberListing -> memberListingScraper


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
