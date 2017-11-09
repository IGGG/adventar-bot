{-# LANGUAGE OverloadedStrings #-}

module Slack where

import           Control.Monad.Except (runExceptT)
import           Data.Text            (Text, unpack)
import           Web.Slack.Types.Id   (Id (Id))
import           Web.Slack.WebAPI     (SlackConfig (SlackConfig),
                                       chat_postMessage)

type Token = Text
type Message = Text
type ChannelName = Text

postMessage :: Token -> ChannelName -> Message -> IO (Either Text ())
postMessage token cname message = runExceptT $
  chat_postMessage (SlackConfig $ unpack token) (Id cname) message []
