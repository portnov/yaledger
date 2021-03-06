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
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts statistics: min, max, avg, quantilies. One optional parameter: account or accounts group."

  initReport _ options _ = setOutputFormat (commonFlags options)
  
  runReport _ qry opts mbPath = stats [qry] opts mbPath
  runReportL _ qrys opts mbPath = stats qrys opts mbPath

commonFlags :: [SOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

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

postingToDouble :: LedgerOptions -> AnyPosting Decimal -> Double
postingToDouble opts (CP p) = (if isAssetPosting opts p then negate else id) $ toDouble $ postingValue p
postingToDouble opts (DP p) = (if isAssetPosting opts p then id else negate) $ toDouble $ postingValue p

isAssetPosting :: LedgerOptions -> Posting v t -> Bool
isAssetPosting opts p = isAssets opts $ accountAttributes $ postingAccount p

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
         => LedgerOptions
         -> [SOptions]
         -> ChartOfAccounts
         -> Maybe ChartOfAccounts
         -> Query
         -> AnyAccount
         -> Ledger l [Ext Double]
loadData lopts opts coa internalGroup qry account
  | Amounts `elem` opts && isJust internalGroup = do
      let Just grp = internalGroup
      balances <- readIOListL (accountBalances account)
      let balances' = reverse $ filter (checkBalance coa internalGroup qry) balances
          entries   = mapMaybe getExtEntry balances'
          eps       = mapMaybe (getEntryPosting account) entries
          postings  = [p | (e,p) <- eps, checkEntryAccs [grp,coa] e]
      let doublePosting ep = mapExt (postingToDouble lopts) ep
      return $ map doublePosting (sort postings)

  | Amounts `elem` opts = do
      postings <- getPostings coa internalGroup qry account
      let doublePosting ep = mapExt (postingToDouble lopts) ep
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
    lopts <- gets lsConfig
    tree <- mapLeafsM (loadData lopts opts coa internalGroup qry) coa
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
  lopts <- gets lsConfig
  lists <- forM queries $ \qry -> loadData lopts options coa internalGroup qry account
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
      format = case selectOutputFormat flags of
                 OASCII _   -> tableColumns ASCII
                 OCSV csv   -> tableColumns csv
                 OHTML html -> tableColumns html
  outputText $ unlinesText $
           format [([output "FROM"],   ALeft,  map (showMaybeDate . srFrom) results'),
                   ([output "TO"],     ALeft,  map (showMaybeDate . srTo)   results'),
                   ([output "OPEN"],   ARight, map (showDouble showCcy ccy . srOpen)   results'),
                   ([output "MIN"],    ARight, map (showDouble showCcy ccy . srMin)    results'),
                   ([output "Q1"],     ARight, map (showDouble showCcy ccy . srQ1)     results'),
                   ([output "MEDIAN"], ARight, map (showDouble showCcy ccy . srMedian) results'),
                   ([output "Q3"],     ARight, map (showDouble showCcy ccy . srQ3)     results'),
                   ([output "MAX"],    ARight, map (showDouble showCcy ccy . srMax)    results'),
                   ([output "AVG"],    ARight, map (showDouble showCcy ccy . srAvg)    results'),
                   ([output "SD"],     ARight, map (showDouble showCcy ccy . srSd)     results'),
                   ([output "CLOSE"],  ARight, map (showDouble showCcy ccy . srClose)  results') ]

byGroup queries options coa = do
    internalGroup <- case [val | SInternal val <- options] of
                       [] -> return Nothing
                       (Nothing:_) -> return $ Just coa
                       (Just grp:_) -> Just <$> getCoAItem (mkPath grp)
    forM_ queries $ \qry -> do
        outputText $ showInterval qry
        go internalGroup qry
  where
    go internalGroup qry = do
      let flags = commonFlags options
      lopts <- gets lsConfig
      srcData <- mapTreeBranchesM (loadGroupData options internalGroup qry)
                                  (loadData lopts options coa internalGroup qry) coa
      let calc = toList . calculate (qStart qry) (qEnd qry) . V.fromList . map getContent . sort
      let results = mapTree calc calc srcData
      let prepare
            | CNoZeros `elem` flags = filterLeafs (any (/= 0.0))
            | otherwise = id
      let showD _ x
            | x < 0 = [Fragment (color Red) (printf "%0.4f" x)]
            | x == 0 = [Fragment faint (printf "%0.4f" x)]
            | otherwise = output $ printf "%0.4f" x
      let format = case selectOutputFormat flags of
                     OASCII _ -> \n qs rs -> unlinesText $ showTreeList [output "ACCOUNT"] (\x -> [x]) showD flags n qs rs
                     OCSV csv -> \n qs rs -> unlinesText $ tableColumns csv (treeTable (\x -> [x]) showD flags n qs rs)
                     OHTML html -> \n qs rs -> unlinesText $ tableColumns html (treeTable (\x -> [x]) showD flags n qs rs)
      let columns = map output ["OPEN", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "AVG", "SD", "CLOSE"]
      outputText $ format (length columns) columns (prepare results)

