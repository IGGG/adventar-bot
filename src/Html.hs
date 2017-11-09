{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Data.Text               (Text, unpack)
import           Entry                   (Url)
import           Test.WebDriver
import           Test.WebDriver.Commands (getSource, openPage)

type Html = Text

fetchHtml :: Url -> IO Html
fetchHtml url = runSession config $ do
  openPage (unpack url)
  getSource
  where
    config = useBrowser chrome defaultConfig
