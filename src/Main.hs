module Main where

import           Data.Aeson         (decode)
import           Data.Maybe         (fromMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.String        (fromString)
import           Scraper
import           System.Environment (getArgs)
import           Types

main :: IO ()
main = do
  [htmlPath, jsonPath] <- getArgs
  htmlFile <- readFile htmlPath
  jsonFile <- readFile jsonPath

  let
    entrys1 = adventerScraper $ fromString htmlFile
    entrys2 = fromMaybe Set.empty . decode $ fromString jsonPath :: Set Entry

  print $ Set.difference entrys2 entrys1
