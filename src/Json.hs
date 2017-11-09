module Json where

import           Data.Aeson               (decode)
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, unpack)
import           Data.Text.Lazy.Builder   (toLazyText)
import           Data.Text.Lazy.Encoding  (encodeUtf8)
import           Entry
import qualified Data.Text.Lazy.IO        as LT

readEntryJson :: Text -> IO Calendar
readEntryJson jsonPath =
  fromMaybe emptyCalender . decode . encodeUtf8 <$> LT.readFile (unpack jsonPath)

updateEntryJson :: Text -> Calendar -> IO ()
updateEntryJson jsonPath newCal =
  LT.writeFile (unpack jsonPath) . toLazyText $ encodePrettyToTextBuilder newCal
