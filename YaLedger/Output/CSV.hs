
module YaLedger.Output.CSV where

import Data.Maybe
import Data.List

import YaLedger.Output.Tables

data CSV = CSV (Maybe String)

csvTable :: Maybe String -> [Column] -> Column
csvTable mbSep lists = map (intercalate sep . map quote) lists
  where
    sep = fromMaybe ";" mbSep

    special = sep ++ "\""
    spaces  = " \r\n\t"

    quote :: String -> String
    quote "" = ""
    quote str
      | any (`elem` spaces) str = "\"" ++ escape str ++ "\""
      | otherwise = escape str

    escape :: String -> String
    escape "" = ""
    escape (c:cs)
      | c `elem` special = '\\': c: escape cs
      | otherwise        = c: escape cs

instance TableFormat CSV where
  tableColumns (CSV sep) list =
    let lists = [(h ++ c) | (h,_,c) <- list]
    in  csvTable sep $ transpose lists

  tableGrid csv colHeaders rows =
    let headers = map snd colHeaders
        rows'   = map padColumns rows
        cols    = foldr1 (zipWith (++)) rows'
    in  tableColumns csv $ zip3 headers (repeat ALeft) cols

