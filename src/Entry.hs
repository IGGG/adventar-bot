{-# LANGUAGE DeriveGeneric #-}

module Entry where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack)
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

dates :: [Date]
dates = fmap (\n -> pack $ mconcat ["12/", twoDigit n, show n]) [1..25]
  where
    twoDigit :: Int -> String
    twoDigit n = if n `mod` 10 /= 0 then "0" else ""

emptyCalender :: Calendar
emptyCalender = HM.empty

data DiffEntry
  = NewEntry Entry
  | UpdateBody Entry
  | RemoveEntry Entry
  | ChangeUser Entry Entry
  | NoChanged
  deriving (Show, Eq)

diff :: Date -> Calendar -> Calendar -> DiffEntry
diff date = diff' `on` HM.lookup date

diff' :: Maybe Entry -> Maybe Entry -> DiffEntry
diff' Nothing Nothing = NoChanged
diff' Nothing (Just entry) = NewEntry entry
diff' (Just entry) Nothing = RemoveEntry entry
diff' (Just e1@(Entry u1 _ _ _)) (Just e2@(Entry u2 _ _ _))
  | e1 == e2  = NoChanged
  | u1 /= u2  = ChangeUser e1 e2
  | otherwise = UpdateBody e2
