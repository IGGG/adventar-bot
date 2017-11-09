{-# LANGUAGE DeriveGeneric #-}

module Entry where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

type Calendar = HashMap Date Entry

data Entry = Entry
  { user    :: Text
  , comment :: Text
  , title   :: Text
  , url     :: Url
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Entry
instance FromJSON Entry

type Url = Text
type Date = Text

emptyCalender :: Calendar
emptyCalender = HM.empty
