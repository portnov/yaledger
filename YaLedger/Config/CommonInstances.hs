
module YaLedger.Config.CommonInstances where

import Data.Dates
import qualified Data.Text as T
import Data.Yaml
import Text.Parsec hiding ((<|>))
import System.IO.Unsafe (unsafePerformIO)

instance FromJSON DateTime where
  parseJSON (String text) =
    let now = unsafePerformIO getCurrentDateTime
    in case parseDate now (T.unpack text) of
         Left err -> fail $ show err
         Right date -> return date
  parseJSON x = fail $ "DateTime: invalid object: " ++ show x

instance FromJSON DateInterval where
  parseJSON (String text) =
    case runParser pDateInterval () (T.unpack text) (T.unpack text) of
      Left err -> fail $ show err
      Right interval -> return interval
  parseJSON _ = fail "Date interval: invalid object"

