{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module YaLedger.Reports.Flow
  (Flow (..)) where

import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Vector as V

import YaLedger.Reports.API
import YaLedger.Reports.Stats as Stats

data Flow = Flow

data FOptions =
       FStats
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
    coaDebit <- getCoAItem (gets lsPosition) (gets lsCoA) p1
    coaCredit <- case p2 of
                   Nothing   -> gets lsCoA
                   Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
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
    debug $ "Selected entries count: " ++ show (length entries)
    let groupped = flatMap $ groupEntries coaDebit entries
    flowStats fullCoA qry flags (FStats `elem` opts) groupped

flowStats coa qry flags showStats grps = do
    let calc = Stats.calculate (qStart qry) (qEnd qry) . V.fromList . map toDouble
        showCcy = CNoCurrencies `notElem` flags
        rows = [(a1,a2, calc xs) | (a1,a2,xs) <- grps]
        format = case needCSV flags of
                   Nothing -> tableColumns ASCII
                   Just sep -> tableColumns (CSV sep)
        debitAcc  (x,_,_) = case needCSV flags of
                             Nothing -> maybe "" (trimPath (maxFieldWidth ASCII)) $ accountFullPath (getID x) coa
                             Just _  -> maybe "" (intercalate "/") $ accountFullPath (getID x) coa
        creditAcc (_,x,_) = case needCSV flags of
                             Nothing -> maybe "" (trimPath (maxFieldWidth ASCII)) $ accountFullPath (getID x) coa
                             Just _  -> maybe "" (intercalate "/") $ accountFullPath (getID x) coa
        thrd (_,_,x) = x
        showF fn (_,crAcc,x) = showDouble showCcy (getCurrency crAcc) (fn x)
    wrapIO $ putStr $ unlines $
             format $ [(["DEBIT"], ALeft, map debitAcc rows),
                       (["CREDIT"], ALeft, map creditAcc rows),
                       (["SUM"], ARight, map (showF srSum) rows)] ++
                       if showStats
                         then [(["MIN"], ARight, map (showF srMin) rows),
                               (["Q1"],  ARight, map (showF srQ1)  rows),
                               (["MEDIAN"], ARight, map (showF srMedian) rows),
                               (["Q3"],  ARight, map (showF srQ3)  rows),
                               (["MAX"], ARight, map (showF srMax) rows),
                               (["AVG"], ARight, map (showF srAvg) rows),
                               (["SD"],  ARight, map (showF srSd)  rows)]
                         else []
