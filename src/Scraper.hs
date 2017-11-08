{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text, pack, unpack)
import           Text.HTML.Scalpel.Core
import           Types

adventerScraper :: Text -> Set Entry
adventerScraper txt = Set.fromList . fromMaybe [] . scrapeStringLike txt $
  chroot ("table" @: [hasClass "mod-entryList"]) (chroots "tr" scrapeEntry)

scrapeEntry :: Scraper Text Entry
scrapeEntry = Entry <$>
  scrapeDate <*> scrapeUser <*> scrapeComment <*> scrapeTitle <*> scrapeUrl

scrapeDate :: Scraper Text Date
scrapeDate = text ("th" @: [hasClass "mod-entryList-date"])

scrapeUser :: Scraper Text Text
scrapeUser = "td" @: [hasClass "mod-entryList-user"] `chroot` text "span"

scrapeComment :: Scraper Text Text
scrapeComment = text ("div" @: [hasClass "mod-entryList-comment"])

scrapeTitle :: Scraper Text Text
scrapeTitle = text ("div" @: [hasClass "mod-entryList-title"])

scrapeUrl :: Scraper Text Url
scrapeUrl = text ("div" @: [hasClass "mod-entryList-url"])
