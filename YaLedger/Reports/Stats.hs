{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Stats
  (Stats (..)) where

import Text.Printf
import qualified Data.Vector as V
import Statistics.Sample
import Statistics.Quantile
import Data.Dates

import YaLedger.Reports.API

data Stats = Stats

data SOptions =
         Amounts
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
       srClose  :: Double }
  deriving (Eq, Show)

data AnyPosting =
       CP (Posting Decimal Credit)
     | DP (Posting Decimal Debit)
  deriving (Eq)

toList :: StatRecord -> [Double]
toList (StatRecord {..}) =
  [srOpen, srMin, srQ1, srMedian, srQ3, srMax, srAvg, srSd, srClose]

showDouble :: [SOptions] -> Currency -> Double -> String
showDouble options c x =
    printf "%0.4f" x ++
    if CNoCurrencies `elem` commonFlags options
      then ""
      else show c

stats queries options mbPath = (do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    case coa of
      Leaf {..} -> byOneAccount queries options leafData
      _         -> byGroup queries options coa )
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

getPostings :: Throws InternalError l => Query -> AnyAccount -> Ledger l [Ext AnyPosting]
getPostings qry account = do
    crp <- filter (checkQuery qry) <$> (readIOListL =<< creditPostings account)
    drp <- filter (checkQuery qry) <$> (readIOListL =<< debitPostings account)
    let toCP (Ext {..}) = Ext {
                            getDate = getDate,
                            extID   = extID,
                            getLocation = getLocation,
                            getAttributes = getAttributes,
                            getContent = CP getContent }
    let toDP (Ext {..}) = Ext {
                            getDate = getDate,
                            extID   = extID,
                            getLocation = getLocation,
                            getAttributes = getAttributes,
                            getContent = DP getContent }
    return $ map toCP crp ++ map toDP drp

postingToDouble :: AnyPosting -> Double
postingToDouble (CP p) = toDouble $ postingValue p
postingToDouble (DP p) = negate $ toDouble $ postingValue p

loadData :: Throws InternalError l => [SOptions] -> Query -> AnyAccount -> Ledger l [Ext Double]
loadData opts qry account
  | Amounts `elem` opts = do
      postings <- getPostings qry account
      let doublePosting (Ext {..}) = Ext {
                                       getDate = getDate,
                                       extID = extID,
                                       getLocation = getLocation,
                                       getAttributes = getAttributes,
                                       getContent = postingToDouble getContent }
      return $ map doublePosting (sort postings)
  | otherwise = do
      balances <- readIOListL (accountBalances account)
      let balances' = reverse $ filter (checkQuery qry) balances
          btype = if CLedgerBalances `elem` commonFlags opts
                     then LedgerBalance
                     else AvailableBalance
          doubleBalance (Ext {..}) = Ext {
                                       getDate = getDate,
                                       extID = extID,
                                       getLocation = getLocation,
                                       getAttributes = getAttributes,
                                       getContent = toDouble (balanceGetter btype getContent) }
      return $ map doubleBalance balances'

loadGroupData :: Throws InternalError l => [SOptions] -> Query -> ChartOfAccounts -> Ledger l [Ext Double]
loadGroupData opts qry coa = do
    tree <- mapLeafsM (loadData opts qry) coa
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
                        srClose  = 0 }
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
                        srClose  = V.head v }
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
                  srClose  = V.last v }

isNotZeroSR :: StatRecord -> Bool
isNotZeroSR sr = any (/= 0.0) (toList sr)

byOneAccount queries options account = do
  lists <- forM queries $ \qry -> loadData options qry account
  let starts = map qStart queries
      ends   = map qEnd   queries
      ccy = getCurrency account
      srcData = map (V.fromList . map getContent . sort) lists
      flags = commonFlags options
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
                   (["OPEN"],   ARight, map (showDouble options ccy . srOpen)   results'),
                   (["MIN"],    ARight, map (showDouble options ccy . srMin)    results'),
                   (["Q1"],     ARight, map (showDouble options ccy . srQ1)     results'),
                   (["MEDIAN"], ARight, map (showDouble options ccy . srMedian) results'),
                   (["Q3"],     ARight, map (showDouble options ccy . srQ3)     results'),
                   (["MAX"],    ARight, map (showDouble options ccy . srMax)    results'),
                   (["AVG"],    ARight, map (showDouble options ccy . srAvg)    results'),
                   (["SD"],     ARight, map (showDouble options ccy . srSd)     results'),
                   (["CLOSE"],  ARight, map (showDouble options ccy . srClose)  results') ]

byGroup queries options coa =
    forM_ queries $ \qry -> do
        wrapIO $ putStrLn $ showInterval qry
        go qry
  where
    go qry = do
      let flags = commonFlags options
      srcData <- mapTreeBranchesM (loadGroupData options qry) (loadData options qry) coa
      let calc = toList . calculate (qStart qry) (qEnd qry) . V.fromList . map getContent . sort
      let results = mapTree calc calc srcData
      let prepare
            | CNoZeros `elem` flags = filterLeafs (any (/= 0.0))
            | otherwise = id
      let showD _ x = printf "%0.4f" x
      let format = case needCSV flags of
                     Nothing  -> showTreeList (\x -> [x]) showD flags
                     Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable id showD flags n qs rs)
      let columns = ["OPEN", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "AVG", "SD", "CLOSE"]
      wrapIO $ putStrLn $ format (length columns) columns (prepare results)

