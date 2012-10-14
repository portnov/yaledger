
module YaLedger.Output.CSV where

import Data.List

csvTable :: String -> [[String]] -> String
csvTable sep lists = unlines $ map (intercalate sep) lists

csvColumns :: String -> [(String, [String])] -> String
csvColumns sep pairs = 
  let lists = [(h:c) | (h,c) <- pairs]
  in  csvTable sep $ transpose lists
