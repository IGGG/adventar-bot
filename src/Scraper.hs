{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import           Control.Applicative    ((<|>))
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Entry
import           Text.HTML.Scalpel.Core

adventarScraper :: Text -> Calendar
adventarScraper txt = HM.fromList . fromMaybe [] . scrapeStringLike txt $
  chroot ("table" @: [hasClass "mod-entryList"]) (chroots "tr" scrapeEntryWithDate)

scrapeEntryWithDate :: Scraper Text (Date, Entry)
scrapeEntryWithDate = (,) <$> scrapeDate <*> scrapeEntry

scrapeEntry :: Scraper Text Entry
scrapeEntry =
  Entry <$> scrapeUser <*> scrapeComment <*> scrapeTitle <*> scrapeUrl

scrapeDate :: Scraper Text Date
scrapeDate = text ("th" @: [hasClass "mod-entryList-date"])

scrapeUser :: Scraper Text Text
scrapeUser = "td" @: [hasClass "mod-entryList-user"] `chroot` text "span"

scrapeComment :: Scraper Text Text
scrapeComment = text ("div" @: [hasClass "mod-entryList-comment"])

scrapeTitle :: Scraper Text Text
scrapeTitle =
  text ("div" @: [hasClass "mod-entryList-title", notHidden]) <|> pure ""

scrapeUrl :: Scraper Text Url
scrapeUrl =
  text ("div" @: [hasClass "mod-entryList-url", notHidden]) <|> pure ""

notHidden :: AttributePredicate
notHidden = notP $ "hidden" @= ""
