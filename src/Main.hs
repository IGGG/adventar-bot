{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson               (decode)
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import           Data.List                (intersperse)
import           Data.Maybe               (fromMaybe)
import           Data.String              (fromString)
import           Data.Text                (pack, unpack)
import           Data.Text.Lazy.Builder   (toLazyText)
import           Data.Text.Lazy.Encoding  (encodeUtf8)
import qualified Data.Text.Lazy.IO        as LT
import           Entry
import           Scraper
import           Slack
import           System.Environment       (getArgs)

main :: IO ()
main = do
  [htmlPath, jsonPath, token] <- getArgs
  jsonFile <- readFile jsonPath
  htmlFile <- readFile htmlPath

  let
    oldCal = fromMaybe emptyCalender . decode . encodeUtf8 $ fromString jsonFile
    newCal = adventerScraper $ fromString htmlFile
    message = unlines . filter ("" /=) $
      (\date -> diffShow date oldCal newCal) <$> dates
    message' = if null message then "No update..." else message

  result <- postMessage (pack token) "bot-test" (pack message')
  case result of
    Right _ -> putStrLn "Success!" >> updateEntryJson jsonPath newCal
    Left  e -> putStrLn $ "Error: " `mappend` unpack e

updateEntryJson :: FilePath -> Calendar -> IO ()
updateEntryJson jsonPath newCal =
  LT.writeFile jsonPath . toLazyText $ encodePrettyToTextBuilder newCal

dates :: [Date]
dates = fmap (\n -> pack $ mconcat ["12/", twoDigit n, show n]) [1..25]
  where
    twoDigit :: Int -> String
    twoDigit n = if n `mod` 10 /= 0 then "0" else ""

diffShow :: Date -> Calendar -> Calendar -> String
diffShow date = (.) (diffShow' date) . diff date

diffShow' :: Date -> DiffEntry -> String
diffShow' date diffEntry =
  case diffEntry of
    NewEntry newEntry ->
      mconcat [unpack date, " [New] ", ppEntry newEntry]
    UpdateBody newEntry ->
      mconcat [unpack date, " [Update] ", ppEntry newEntry]
    RemoveEntry oldEntry ->
      mconcat [unpack date, " [Remove] ", ppEntry oldEntry]
    ChangeUser oldEntry newEntry ->
      mconcat . intersperse "\n" $
        diffShow' date <$> [RemoveEntry oldEntry, NewEntry newEntry]
    NoChanged -> ""

ppEntry :: Entry -> String
ppEntry (Entry user' comment' title' url') =
  mconcat [mkBody comment' title' url', " by ", unpack user']
  where
    mkBody "" _ ""          = unpack "No Comment..."
    mkBody comment_ _ ""    = unpack comment_
    mkBody comment_ "" url_ = unpack $ mconcat ["<", url_, "|", comment_, ">"]
    mkBody _ title_ url_    = unpack $ mconcat ["<", url_, "|", title_, ">"]
