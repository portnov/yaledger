{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Stats
  (Stats (..),
   StatRecord (..),
   calculate, toDouble, toList) where

import Text.Printf
import qualified Data.Vector as V
import Statistics.Sample
import Statistics.Quantile
import Data.Dates

import YaLedger.Reports.API

data Stats = Stats

data SOptions =
         Amounts
       | SInternal (Maybe String)
       | Common CommonFlags
  deriving (Eq, Show)

instance ReportClass Stats where
  type Options Stats = SOptions
  type Parameters Stats = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg $ Common CNoZeros) "Do not show records for accounts/periods with no postings",
     Option "g" ["hide-groups"] (NoArg $ Common CHideGroups) "Hide accounts groups in CSV output",
     Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "A" ["amounts"] (NoArg Amounts) "Show amounts statistics instead of balances statistics",
     Option "l" ["ledger"] (NoArg $ Common CLedgerBalances) "Show ledger balances instead of available balances",
     Option "i" ["internal"] (OptArg SInternal "GROUP") "Show only entries where all accounts belong to GROUP",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts statistics: min, max, avg, quantilies. One optional parameter: account or accounts group."
  
  runReport _ qry opts mbPath = stats [qry] opts mbPath
  runReportL _ qrys opts mbPath = stats qrys opts mbPath

commonFlags :: [SOptions] -> [CommonFlags]
commonFlags opts = mapMaybe get opts
  where
    get (Common f) = Just f
    get _ = Nothing

toDouble :: Decimal -> Double
toDouble d = fromIntegral (decimalMantissa d) / (10 ^ decimalPlaces d)

data StatRecord = StatRecord {
       srFrom   :: Maybe DateTime,
       srTo     :: Maybe DateTime,
       srOpen   :: Double,
       srMin    :: Double,
       srQ1     :: Double,
       srMedian :: Double,
       srQ3     :: Double,
       srMax    :: Double,
       srAvg    :: Double,
       srSd     :: Double,
       srClose  :: Double,
       srSum    :: Double }
  deriving (Eq, Show)

toList :: StatRecord -> [Double]
toList (StatRecord {..}) =
  [srOpen, srMin, srQ1, srMedian, srQ3, srMax, srAvg, srSd, srClose]

stats queries options mbPath = (do
    coa <- getCoAItemL mbPath
    case coa of
      Leaf {..} -> byOneAccount coa queries options leafData
      _         -> byGroup queries options coa )
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

checkPosting :: ChartOfAccounts -> Maybe ChartOfAccounts -> Query -> Ext (Posting Decimal t) -> Bool
checkPosting _ Nothing qry ep = checkQuery qry ep
checkPosting coa (Just grp) qry ep =
    checkQuery qry ep &&
    any (postingAccount (getContent ep) `isInCoA`) [grp,coa]

getPostings :: Throws InternalError l
            => ChartOfAccounts
            -> Maybe ChartOfAccounts
            -> Query -> AnyAccount -> Ledger l [Ext (AnyPosting Decimal)]
getPostings coa internalGroup qry account = do
    crp <- filter (checkPosting coa internalGroup qry) <$> (readIOListL =<< creditPostings account)
    drp <- filter (checkPosting coa internalGroup qry) <$> (readIOListL =<< debitPostings account)
    let toCP ep = mapExt CP ep
    let toDP ep = mapExt DP ep
    return $ map toCP crp ++ map toDP drp

postingToDouble :: AnyPosting Decimal -> Double
postingToDouble (CP p) = toDouble $ postingValue p
postingToDouble (DP p) = negate $ toDouble $ postingValue p

getEntryPosting :: AnyAccount -> Ext (Entry Decimal Checked) -> Maybe (Entry Decimal Checked, Ext (AnyPosting Decimal))
getEntryPosting acc ee@(Ext {getContent = e}) =
    case map CP (filter check $ cEntryCreditPostings e) ++ map DP (filter check $ cEntryDebitPostings e) of
      [] -> Nothing
      (p:_) -> Just (e, mapExt (const p) ee)
  where
    check :: Posting Decimal t -> Bool
    check p = getID (postingAccount p) == getID acc

getExtEntry :: Ext (Balance Checked) -> Maybe (Ext (Entry Decimal Checked))
getExtEntry extBal@(Ext {..}) =
  case causedBy getContent of
    Nothing -> Nothing
    Just e -> Just $ mapExt (const e) extBal

loadData :: Throws InternalError l
         => [SOptions]
         -> ChartOfAccounts
         -> Maybe ChartOfAccounts
         -> Query
         -> AnyAccount
         -> Ledger l [Ext Double]
loadData opts coa internalGroup qry account
  | Amounts `elem` opts && isJust internalGroup = do
      let Just grp = internalGroup
      balances <- readIOListL (accountBalances account)
      let balances' = reverse $ filter (checkBalance coa internalGroup qry) balances
          entries   = mapMaybe getExtEntry balances'
          eps       = mapMaybe (getEntryPosting account) entries
          postings  = [p | (e,p) <- eps, checkEntryAccs [grp,coa] e]
      let doublePosting ep = mapExt postingToDouble ep
      return $ map doublePosting (sort postings)

  | Amounts `elem` opts = do
      postings <- getPostings coa internalGroup qry account
      let doublePosting ep = mapExt postingToDouble ep
      return $ map doublePosting (sort postings)

  | otherwise = do
      balances <- readIOListL (accountBalances account)
      let balances' = reverse $ filter (checkBalance coa internalGroup qry) balances
          btype = if CLedgerBalances `elem` commonFlags opts
                     then LedgerBalance
                     else AvailableBalance
          doubleBalance extBal = mapExt (\bal -> toDouble (balanceGetter btype bal)) extBal
      return $ map doubleBalance balances'

loadGroupData :: Throws InternalError l
              => [SOptions] 
              -> Maybe ChartOfAccounts
              -> Query
              -> ChartOfAccounts
              -> Ledger l [Ext Double]
loadGroupData opts internalGroup qry coa = do
    tree <- mapLeafsM (loadData opts coa internalGroup qry) coa
    return $ concat $ allLeafs tree

calculate :: Maybe DateTime -> Maybe DateTime -> V.Vector Double -> StatRecord
calculate d1 d2 v
  | V.length v == 0 = StatRecord {
                        srFrom   = d1,
                        srTo     = d2,
                        srOpen   = 0,
                        srMin    = 0,
                        srQ1     = 0,
                        srMedian = 0,
                        srQ3     = 0,
                        srMax    = 0,
                        srAvg    = 0,
                        srSd     = 0,
                        srClose  = 0,
                        srSum    = 0 }
  | V.length v == 1 = StatRecord {
                        srFrom   = d1,
                        srTo     = d2,
                        srOpen   = V.head v,
                        srMin    = V.head v,
                        srQ1     = V.head v,
                        srMedian = V.head v,
                        srQ3     = V.head v,
                        srMax    = V.head v,
                        srAvg    = V.head v,
                        srSd     = 0,
                        srClose  = V.head v,
                        srSum    = V.head v }
  | otherwise = StatRecord {
                  srFrom   = d1,
                  srTo     = d2,
                  srOpen   = V.head v,
                  srMin    = V.minimum v,
                  srQ1     = continuousBy cadpw 1 4 v,
                  srMedian = continuousBy cadpw 2 4 v,
                  srQ3     = continuousBy cadpw 3 4 v,
                  srMax    = V.maximum v,
                  srAvg    = mean v,
                  srSd     = stdDev v,
                  srClose  = V.last v,
                  srSum    = V.sum v }

isNotZeroSR :: StatRecord -> Bool
isNotZeroSR sr = any (/= 0.0) (toList sr)

byOneAccount coa queries options account = do
  internalGroup <- case [val | SInternal val <- options] of
                     [] -> return Nothing
                     (Nothing:_) -> return $ Just coa
                     (Just grp:_) -> Just <$> getCoAItem (mkPath grp)
  lists <- forM queries $ \qry -> loadData options coa internalGroup qry account
  let starts = map qStart queries
      ends   = map qEnd   queries
      ccy = getCurrency account
      srcData = map (V.fromList . map getContent . sort) lists
      flags = commonFlags options
      showCcy = CNoCurrencies `notElem` flags
      results = zipWith3 calculate starts ends srcData
      prepare = if CNoZeros `elem` flags
                  then filter isNotZeroSR
                  else id
      results' = prepare results
      format = case needCSV flags of
                 Nothing  -> tableColumns ASCII
                 Just sep -> tableColumns (CSV sep)
  wrapIO $ putStr $ unlines $
           format [(["FROM"],   ALeft,  map (showMaybeDate . srFrom) results'),
                   (["TO"],     ALeft,  map (showMaybeDate . srTo)   results'),
                   (["OPEN"],   ARight, map (showDouble showCcy ccy . srOpen)   results'),
                   (["MIN"],    ARight, map (showDouble showCcy ccy . srMin)    results'),
                   (["Q1"],     ARight, map (showDouble showCcy ccy . srQ1)     results'),
                   (["MEDIAN"], ARight, map (showDouble showCcy ccy . srMedian) results'),
                   (["Q3"],     ARight, map (showDouble showCcy ccy . srQ3)     results'),
                   (["MAX"],    ARight, map (showDouble showCcy ccy . srMax)    results'),
                   (["AVG"],    ARight, map (showDouble showCcy ccy . srAvg)    results'),
                   (["SD"],     ARight, map (showDouble showCcy ccy . srSd)     results'),
                   (["CLOSE"],  ARight, map (showDouble showCcy ccy . srClose)  results') ]

byGroup queries options coa = do
    internalGroup <- case [val | SInternal val <- options] of
                       [] -> return Nothing
                       (Nothing:_) -> return $ Just coa
                       (Just grp:_) -> Just <$> getCoAItem (mkPath grp)
    forM_ queries $ \qry -> do
        wrapIO $ putStrLn $ showInterval qry
        go internalGroup qry
  where
    go internalGroup qry = do
      let flags = commonFlags options
      srcData <- mapTreeBranchesM (loadGroupData options internalGroup qry)
                                  (loadData options coa internalGroup qry) coa
      let calc = toList . calculate (qStart qry) (qEnd qry) . V.fromList . map getContent . sort
      let results = mapTree calc calc srcData
      let prepare
            | CNoZeros `elem` flags = filterLeafs (any (/= 0.0))
            | otherwise = id
      let showD _ x = printf "%0.4f" x
      let format = case needCSV flags of
                     Nothing  -> showTreeList ["ACCOUNT"] (\x -> [x]) showD flags
                     Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable id showD flags n qs rs)
      let columns = ["OPEN", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "AVG", "SD", "CLOSE"]
      wrapIO $ putStrLn $ format (length columns) columns (prepare results)

