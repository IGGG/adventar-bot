module Main where

import           Data.Aeson         (decode)
import           Data.Maybe         (fromMaybe)
import           Data.String        (fromString)
import           Scraper
import           System.Environment (getArgs)
import           Entry

main :: IO ()
main = do
  [htmlPath, jsonPath] <- getArgs
  htmlFile <- readFile htmlPath
  jsonFile <- readFile jsonPath

  let
    entrys1 = adventerScraper $ fromString htmlFile
    entrys2 = fromMaybe emptyCalender . decode $ fromString jsonPath

  print entrys1
  print entrys2
