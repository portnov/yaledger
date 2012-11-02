
module YaLedger.Output.Messages where

import Data.Maybe

import YaLedger.Types.Common

formatMessage :: [(String, String)] -> MessageFormat -> String
formatMessage pairs format = concatMap go format
  where
    go (MFixed str) = str
    go (MVariable name) = fromMaybe ('#':name) (lookup name pairs)

