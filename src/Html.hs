{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Data.Text               (Text, unpack)
import           Entry                   (Url)
import           Test.WebDriver
import           Test.WebDriver.Commands (getSource, openPage)

type Html = Text

fetchHtml :: Text -> Text -> Url -> IO Html
fetchHtml host port url = runSession config $ do
  openPage (unpack url)
  getSource
  where
    config = useBrowser chrome $ 
      defaultConfig { wdHost = unpack host, wdPort = read (unpack port) }
