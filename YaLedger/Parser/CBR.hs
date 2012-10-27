{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module YaLedger.Parser.CBR
  (loadCBR, test
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Data.Yaml
import Data.Dates
import Network.HTTP
import Network.Browser
import Text.Printf
import Text.Parsec

import YaLedger.Types hiding (getChildren)
import YaLedger.Parser.Common (loadParserConfig)
import YaLedger.Output.Pretty
import YaLedger.Config () -- we need some instances for FromJSON
import YaLedger.Parser.Currencies ()
import qualified YaLedger.Parser.Transactions as T

roubles :: Currency
roubles = Currency {
  cSymbol = "р",
  cIntCode = Just 643,
  cStrCode = Just "RUB",
  cPrecision = 2 }

type ParserConfig = [GetRate]

data GetRate = GetRate {
  currencyName :: String,
  currencyCode :: Int,
  readAsReversible :: Bool,
  startDate :: DateTime,
  checkInterval :: DateInterval }
  deriving (Eq, Show)

instance Ord GetRate where
  compare gr1 gr2 = compare (currencyName gr1) (currencyName gr2)

instance FromJSON GetRate where
  parseJSON (Object v) =
    GetRate
      <$> v .: "currency"
      <*> v .: "code"
      <*> v .:? "reversible" .!= False
      <*> v .:  "start-date"
      <*> v .:? "check-interval" .!= Weeks 1
  parseJSON _ = fail "CBR rate descriptor: invalid object"

checks :: DateTime -> GetRate -> [(DateTime, GetRate)]
checks till gr = go (startDate gr)
  where
    go date
      | date < till = (date, gr): go (date `addInterval` checkInterval gr)
      | otherwise   = []

splitP :: (a -> Bool) -> [a] -> ([a], [a])
splitP _ [] = ([], [])
splitP p list = go [] [] list
  where
    go good bad [] = (good, bad)
    go good bad (x:xs)
      | p x = go (x:good) bad xs
      | otherwise = go good (x:bad) xs

allChecks :: DateTime -> ParserConfig -> [(DateTime, String)] -> [(DateTime, [GetRate])]
allChecks till pc old = mrg $ sort $ filter new $ concatMap (checks till) pc
  where
    new (d,gr) = (d, currencyName gr) `notElem` old
    mrg [] = []
    mrg [(d,c)] = [(d, [c])]
    mrg ((d1,c1):xs) =
      let (sameDate, other) = splitP ((== d1) . fst) xs
      in  (d1, c1: map snd sameDate): mrg other

is ::  ArrowXml cat => String -> cat XmlTree XmlTree
is name = isElem >>> hasName name

getRateStrings :: (ArrowXml cat) => [Int] -> cat (NTree XNode) (String, (String, String))
getRateStrings cids =
    getChildren >>> is "ValCurs" /> is "Valute" >>> filterA check >>> (code &&& (nominal &&& rate))
  where
    check = getChildren >>> is "NumCode" /> hasText (`elem` map show cids)

    code = getChildren >>> is "NumCode" /> getText

    nominal = getChildren >>> is "Nominal" /> getText

    rate = getChildren >>> is "Value" /> getText

-- | HACK around <? xml encoding='windows-1251' ?>
skipXmlDecl :: String -> String
skipXmlDecl str = go 0 str
  where
    go _ [] = []
    go 0 ('<':xs) = go 1 xs
    go 1 ('?':xs) = go 2 xs
    go 2 (x:xs)
      | x == '?'  = go 3 xs
      | otherwise = go 2 xs
    go 3 ('>':xs) = xs
    go 3 (x:xs) = go 2 xs
    go _ _ = error "Impossible: skipXmlDecl"

-- | Get CBR XML exchange rates description
getCBRXML :: DateTime -> IO String
getCBRXML date = do
  let src :: String
      src = printf "http://www.cbr.ru/scripts/XML_daily.asp?date_req=%02d/%02d/%04d"
                   (day date) (month date) (year date)
  (_, rsp) <- browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest src
  return $ skipXmlDecl (rspBody rsp)

parseRates :: String -> [Int] -> IO [(Int, (Double, Double))]
parseRates str cids = do
    pairs <- runX $ readString [] str >>> getRateStrings cids
    return $ map (read *** (double *** double)) pairs
  where
    double s = read (dot s)

    dot [] = []
    dot (',':s) = '.':s
    dot (x:xs) = x: dot xs

pRates :: T.Parser [Ext Record]
pRates =
  T.ext T.pSetRate `sepEndBy` many newline

loadCache :: Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadCache currs coa cachePath = do
  now <- getCurrentDateTime
  let !st = T.emptyPState now coa currs (Just "YYYY/MM/DD")
  content <- readFile cachePath
  case runParser pRates st cachePath content of
    Left err -> fail $ show err
    Right recs -> return recs

getChecks :: [Ext Record] -> [(DateTime, String)]
getChecks recs = concatMap go recs
  where
    go (Ext date _ _ (SetRate rates)) =
      [(date, cSymbol $ rateCurrencyFrom r) | r <-  rates]
    go _ = error "Impossible: CBR.getChecks.go"

loadCBR :: FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadCBR configPath currs coa cachePath = do
    config <- loadParserConfig configPath
    cache <- loadCache currs coa cachePath
    let old = getChecks cache
    now <- getCurrentDateTime
    new <- forM (allChecks now config old) $ \(date, grs) -> do
             doc <- getCBRXML date
             pairs <- parseRates doc (map currencyCode grs)
             let rates = map (bind grs) pairs
             return $ Ext date nowhere M.empty $ SetRate $
                        map (convert currs) rates
    let records = cache ++ new
    writeFile cachePath $ unlines $ map prettyPrint records
    return records
  where
    nowhere = newPos "<nowhere>" 0 0
    convert currs (cFromS, rev, (aFrom, aTo)) =
      case M.lookup cFromS currs of
        Nothing -> error $ "While loading CBR rates: unknown currency: " ++ cFromS
        Just cFrom -> Explicit cFrom aFrom roubles aTo rev
    bind grs (cid, rate) =
      case filter ((== cid) . currencyCode) grs of
        [gr] -> (currencyName gr, readAsReversible gr, rate)
        _ -> error "Impossible: CBR.loadCBR.bind"

test :: IO ()
test = do
  str <- getCBRXML (DateTime 2012 10 03 0 0 0)
  rates <- parseRates str [840, 978]
  forM_ rates print

