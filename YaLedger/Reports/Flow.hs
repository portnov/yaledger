{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module YaLedger.Reports.Flow
  (Flow (..),
   EntriesMap,
   groupEntries,
   flatMap,
   formatDot) where

import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Vector as V
import Text.Printf

import YaLedger.Reports.API
import YaLedger.Reports.Stats as Stats

data Flow = Flow

data FOptions =
       FStats
     | FDot
     | Common CommonFlags
  deriving (Eq, Show)

type TwoPaths = (Path, Maybe Path)

instance ReportParameter TwoPaths where
  parseParameter = do
    p1 <- parseParameter
    p2 <- parseParameter
    return (p1, p2)

instance ReportClass Flow where
  type Options Flow = FOptions
  type Parameters Flow = TwoPaths
  reportOptions _ = 
    [Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "s" ["stats"] (NoArg FStats) "Show statistics",
     Option "D" ["dot"] (NoArg FDot) "Output data (only sums) in DOT format (Graphviz)",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show money flow between two groups of accounts. Parameters:\n" ++
                 "  FROM: mandatory. Account or group FROM money are going.\n" ++
                 "  TO: optional. Account or group TO money are going. All accounts, if not specified."

  runReport _ qry opts (p1,p2) = flow qry opts p1 p2

getCoABalances qry coa = do
  balLists <- mapM readIOListL (map accountBalances $ allLeafs coa)
  return $ mergeBalances $ map (filter (checkQuery qry)) (balLists :: [[Ext (Balance Checked)]])

mergeBalances lists = nubBy ((==) `on` extEntry) $ sort $ concat lists
  where
    extEntry extBal =
      case causedBy (getContent extBal) of
        Nothing -> Nothing
        Just entry -> Just $ mapExt (const entry) extBal

flow qry options p1 p2 = (do
    fullCoA <- gets lsCoA
    coaDebit <- getCoAItemL (Just p1)
    coaCredit <- getCoAItemL p2
    flowCoA fullCoA qry options coaDebit coaCredit )
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

checkCreditAcc coa e =
  any (`isInCoA` coa) $ map postingAccount $ cEntryCreditPostings e

type EntriesMap = M.Map AnyAccount (M.Map AnyAccount [Decimal])

groupEntries :: ChartOfAccounts -> [Entry Decimal Checked] -> EntriesMap
groupEntries coa es = foldr insertEntry M.empty es
  where
    insertEntry :: Entry Decimal Checked -> EntriesMap -> EntriesMap
    insertEntry e m =
      let drAccs = filter (`isInCoA` coa) $ map postingAccount $ cEntryDebitPostings e
      in  foldr (insertAcc e) m drAccs

    insertAcc :: Entry Decimal Checked -> AnyAccount -> EntriesMap -> EntriesMap
    insertAcc e acc m =
      foldr (\p -> insertAmount acc (postingValue p) (postingAccount p)) m (cEntryCreditPostings e)

    insertAmount :: AnyAccount -> Decimal -> AnyAccount -> EntriesMap -> EntriesMap
    insertAmount drAcc x crAcc m =
      M.insertWith insertOuter drAcc (M.singleton crAcc [x]) m

    insertOuter m1 m2 = M.unionWith (++) m1 m2

flatMap :: EntriesMap -> [(AnyAccount, AnyAccount, [Decimal])]
flatMap m = concatMap (uncurry go) $ M.assocs (M.map (sortCredits . M.assocs) m)
  where
    sortCredits :: [(AnyAccount, [Decimal])] -> [(AnyAccount, [Decimal])]
    sortCredits = sortBy (compare `on` (negate . sum . snd))

    go :: AnyAccount -> [(AnyAccount, [Decimal])] -> [(AnyAccount, AnyAccount, [Decimal])]
    go acc list = [(acc, a2, xs) | (a2, xs) <- list]

flowCoA fullCoA qry opts coaDebit coaCredit = do
    let flags = [f | Common f <- opts]
    balsDebit  <- getCoABalances qry coaDebit
    let entriesDebit  = mapMaybe (causedBy . getContent) balsDebit
        entries = filter (checkCreditAcc coaCredit) entriesDebit
    $debug $ "Selected entries count: " ++ show (length entries)
    let groupped = flatMap $ groupEntries coaDebit entries
    flowStats fullCoA qry flags (FStats `elem` opts) (FDot `elem` opts) groupped

flowStats coa qry flags showStats asDot grps = do
    let calc = Stats.calculate (qStart qry) (qEnd qry) . V.fromList . map toDouble
        showCcy = CNoCurrencies `notElem` flags
        rows = [(a1,a2, calc xs) | (a1,a2,xs) <- grps]
        format = case needCSV flags of
                   Nothing -> tableColumns ASCII
                   Just sep -> tableColumns (CSV sep)
        debitAcc  (x,_,_) = case needCSV flags of
                             Nothing -> output $ maybe "" (trimPath (maxFieldWidth ASCII)) $ accountFullPath (getID x) coa
                             Just _  -> output $ maybe "" (intercalate "/") $ accountFullPath (getID x) coa
        creditAcc (_,x,_) = case needCSV flags of
                             Nothing -> output $ maybe "" (trimPath (maxFieldWidth ASCII)) $ accountFullPath (getID x) coa
                             Just _  -> output $ maybe "" (intercalate "/") $ accountFullPath (getID x) coa
        thrd (_,_,x) = x
        showF fn (_,crAcc,x) = showDouble showCcy (getCurrency crAcc) (fn x)
    if asDot
      then outputString $ unlines $ formatDot srSum double2int coa rows
      else outputText $ unlinesText $
             format $ [([output "DEBIT"], ALeft, map debitAcc rows),
                       ([output "CREDIT"], ALeft, map creditAcc rows),
                       ([output "SUM"], ARight, map (showF srSum) rows)] ++
                       if showStats
                         then [([output "MIN"], ARight, map (showF srMin) rows),
                               ([output "Q1"],  ARight, map (showF srQ1)  rows),
                               ([output "MEDIAN"], ARight, map (showF srMedian) rows),
                               ([output "Q3"],  ARight, map (showF srQ3)  rows),
                               ([output "MAX"], ARight, map (showF srMax) rows),
                               ([output "AVG"], ARight, map (showF srAvg) rows),
                               ([output "SD"],  ARight, map (showF srSd)  rows)]
                         else []

double2int :: Double -> Integer
double2int x = round (x * 100)

formatDot :: (Show d) => (s -> d) -> (d -> Integer) -> ChartOfAccounts -> [(AnyAccount, AnyAccount, s)] -> [String]
formatDot fn toInt coa rows =
    ["digraph G {"] ++
    ["  rankdir = LR;"] ++
    map node (nub $ sort $ map fst3 rows ++ map snd3 rows) ++
    map edge (map sum3 rows) ++
    ["}"]
  where
    fst3 (x,_,_) = x
    snd3 (_,x,_) = x
    sum3 (x,y,zs) = (x,y, fn zs)
    
    node acc = printf "  \"%d\" [label=\"%s\"];" (getID acc) path
      where path = maybe "" (intercalate "/") $ accountFullPath (getID acc) coa

    edge (acc1, acc2, x) =
      printf "  \"%d\" -> \"%d\" [label=\"%s\", weight=%d];" (getID acc1) (getID acc2) (show x) (toInt x)

