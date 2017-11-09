{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception  (IOException, catch)
import           Control.Monad      (when)
import           Data.Either        (isRight)
import           Data.List          (intersperse)
import           Data.Text          (Text, pack, unpack)
import           Entry
import           Html
import           Json
import           Scraper
import           Slack
import           System.Environment (getArgs, getEnv)

import           GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  [htmlUrl, jsonPath, channel] <- fmap pack <$> getArgs
  [token, wdHost, wdPort] <- 
    fmap pack <$> mapM getEnv ["SLACK_TOKEN", "WD_HOST", "WD_PORT"]
  catch (runBot jsonPath htmlUrl (wdHost, wdPort) (token, channel)) $
    \e -> putStrLn ("Error: " `mappend` show (e :: IOException))

runBot :: Text -> Url -> (Text, Text) -> (Token, ChannelName) -> IO ()
runBot jsonPath htmlUrl (wdHost, wdPort) (token, channel) = do
  oldCal <- readEntryJson jsonPath
  newCal <- adventarScraper <$> fetchHtml wdHost wdPort htmlUrl

  let
    message = mkMessage oldCal newCal

  result <- postMessage token channel (pack $ either id id message)
  case result of
    Right _ -> putStrLn "Success!"
    Left  e -> putStrLn $ "Error: " `mappend` unpack e

  when (isRight message && isRight result) $ updateEntryJson jsonPath newCal


mkMessage :: Calendar -> Calendar -> Either String String
mkMessage oldCal newCal =
  if null message then Left "No update..." else Right message
  where
    message = unlines . filter ("" /=) $ mkMessage' <$> dates
    mkMessage' date = diffShow date oldCal newCal

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
