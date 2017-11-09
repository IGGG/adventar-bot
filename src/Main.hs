{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List          (intersperse)
import           Data.Text          (pack, unpack)
import           Entry
import           Html
import           Json
import           Scraper
import           Slack
import           System.Environment (getArgs)

main :: IO ()
main = do
  [htmlUrl, jsonPath, token] <- fmap pack <$> getArgs

  oldCal <- readEntryJson jsonPath
  newCal <- adventerScraper <$> fetchHtml htmlUrl

  let
    message = unlines . filter ("" /=) $
      (\date -> diffShow date oldCal newCal) <$> dates
    message' = if null message then "No update..." else message

  result <- postMessage token "bot-test" (pack message')
  case result of
    Right _ -> putStrLn "Success!" >> updateEntryJson jsonPath newCal
    Left  e -> putStrLn $ "Error: " `mappend` unpack e


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
