module Types where

import           Data.Text (Text)

data Entry = Entry
  { date    :: Date
  , user    :: Text
  , comment :: Text
  , title   :: Text
  , url     :: Url
  } deriving (Show, Eq, Ord)

type Url = Text

type Date = Text
