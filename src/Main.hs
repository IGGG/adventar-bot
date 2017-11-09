module Main where

import           Data.Aeson         (decode)
import           Data.Maybe         (fromMaybe)
import           Data.String        (fromString)
import           Scraper
import           System.Environment (getArgs)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Entry

main :: IO ()
main = do
  [htmlPath, jsonPath] <- getArgs
  jsonFile <- readFile jsonPath
  htmlFile <- readFile htmlPath

  let
    oldCal = fromMaybe emptyCalender . decode . encodeUtf8 $ fromString jsonFile
    newCal = adventerScraper $ fromString htmlFile

  print entrys1
  print entrys2
