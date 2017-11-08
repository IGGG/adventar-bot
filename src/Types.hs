{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Entry = Entry
  { date    :: Date
  , user    :: Text
  , comment :: Text
  , title   :: Text
  , url     :: Url
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Entry
instance FromJSON Entry

type Url = Text
type Date = Text
