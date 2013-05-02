{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}

module YaLedger.Reports.Common where

import Data.Maybe
import Data.List
import Data.Decimal
import Data.Dates
import Data.String
import Text.Printf

import YaLedger.Types
import YaLedger.Output
import YaLedger.Output.ANSI
import YaLedger.Kernel

data CommonFlags =
    CNoZeros
  | CHideGroups
  | COnlyPositive
  | COnlyNegative
  | CAbsoluteValues
  | CLedgerBalances
  | CBothBalances
  | CNoCurrencies
  | CCSV (Maybe String)
  deriving (Eq, Show)

listTable :: (ToTable opts a, TableFormat fmt) => fmt -> opts -> [a] -> Column
listTable fmt opts list =
    tableColumns fmt [(columnTitle col, columnAlign col, map (concat . columnGetter col) list) | col <- shownColumns opts]

listGrid :: (ToTable opts a, TableFormat fmt) => fmt -> opts -> [a] -> Column
listGrid fmt opts list =
    tableGrid fmt [(columnAlign col, columnTitle col) | col <- cols]
                  [[columnGetter col item | col <- cols] | item <- list]
  where
    cols = shownColumns opts

needCSV :: [CommonFlags] -> Maybe (Maybe String)
needCSV opts =
  case [s | CCSV s <- opts] of
    [] -> Nothing
    (x:_) -> Just x

checkBalance :: ChartOfAccounts -> Maybe ChartOfAccounts -> Query -> Ext (Balance Checked) -> Bool
checkBalance coa onlyGroup qry extBal =
    case onlyGroup of
      Just grp -> case causedBy (getContent extBal) of
                       Nothing -> False
                       Just entry -> checkQuery qry extBal && checkEntryAccs [grp, coa] entry
      Nothing -> checkQuery qry extBal

checkExtEntry :: ChartOfAccounts -> Maybe ChartOfAccounts -> Query -> Ext (Entry Decimal Checked) -> Bool
checkExtEntry _ Nothing qry e = checkQuery qry e
checkExtEntry coa (Just grp) qry e = checkQuery qry e && checkEntryAccs [grp, coa] (getContent e)

checkEntryAccs :: [ChartOfAccounts] -> Entry Decimal Checked -> Bool
checkEntryAccs coas e =
    checkPostings coas (cEntryCreditPostings e) && checkPostings coas (cEntryDebitPostings e)
  where
    checkPostings :: [ChartOfAccounts] -> [Posting Decimal t] -> Bool
    checkPostings coas ps = all (\p -> any (postingAccount p `isInCoA`) coas) ps

datesBackFrom :: DateTime -> DateInterval -> [DateTime]
datesBackFrom date int = go date
  where
    go dt = dt: go (dt `minusInterval` int)

datesBetween :: DateTime -> DateTime -> DateInterval -> [DateTime]
datesBetween start end int =
  reverse $ takeWhile (>= start) $ datesBackFrom end int

intervalsBetween :: DateTime -> DateTime -> DateInterval -> [(DateTime, DateTime)]
intervalsBetween start end int =
  let dates = datesBetween start end int
      first = if head dates > start
                then [(start, head dates)]
                else []
  in  first ++ zip dates (tail dates)
      
splitQuery :: DateTime -> DateTime -> Query -> DateInterval -> [Query]
splitQuery first now qry int =
  let start = fromMaybe first (qStart qry)
      end   = fromMaybe now   (qEnd   qry)
      pairs = intervalsBetween start end int
      attrs = qAttributes qry
      adm   = qAllAdmin qry
  in  map (\(s, e) -> Query (Just s) (Just e) adm attrs) pairs

trimPath :: Maybe Int -> Path -> String
trimPath Nothing p = intercalate "/" p
trimPath (Just n) ps =
  let rlist = reverse ps
  in  case takeWhile ((< n) . snd) $ zip rlist $ tail $ scanl (+) 0 $ map length rlist of
        [] -> last ps
        xs -> intercalate "/" (reverse $ map fst xs)

showDouble :: Bool -> Currency -> Double -> FormattedText
showDouble showCurrencies c x =
  let fmt = "%0." ++ show (cPrecision c) ++ "f"
      str = printf fmt x ++
              if showCurrencies
                then show c
                else ""
  in  if x < 0
        then [Fragment (color Red) str]
        else if x == 0
             then [Fragment faint str]
             else output str

treeTable :: (q -> FormattedText) -> ([CommonFlags] -> a -> FormattedText) -> [CommonFlags] -> Int -> [q] -> Tree [a] [a] -> [(Column, Align, Column)]
treeTable showQry showX options n qrys tree =
  let paths = map fromString $ map (intercalate "/") $ getPaths tree
      hideGroups = CHideGroups `elem` options
      cols = [map (\l -> showX options (l !! i)) (getNodes tree) | i <- [0..n-1]]
      getPaths = if hideGroups then allLeafPaths else allPaths
      getNodes = if hideGroups then allLeafs else allNodes
  in  (["ACCOUNT"], ALeft, paths):
      [([showQry qry], ALeft, col) | (col, qry) <- zip cols qrys]
   
showTreeList :: Show a => Column -> (q -> [FormattedText]) -> ([CommonFlags] -> a -> FormattedText) -> [CommonFlags] -> Int -> [q] -> Tree [a] [a] -> Column
showTreeList title showQry showX options n qrys tree =
  let struct = showTreeStructure tree
      cols = [map (\l -> showX options (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  tableColumns ASCII $
              (title, ALeft, struct):
              [(showQry qry, ARight, col) | (col,qry) <- zip cols qrys]

