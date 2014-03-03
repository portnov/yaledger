{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies, TemplateHaskell #-}

module YaLedger.Reports.Rates
  (RatesReport (..)) where

import Text.Printf
import YaLedger.Reports.API

data RatesReport = RatesReport

data ROptions =
    RForwardOnly
  | Common CommonFlags
  deriving (Eq, Show)

data RParams = RParams {
                rFrom :: Maybe String,
                rTo :: Maybe String }
              deriving (Eq, Show)

instance ReportParameter RParams where
  parseParameter = do
    p1 <- parseParameter
    p2 <- parseParameter
    return $ RParams p1 p2

instance ReportClass RatesReport where
  type Options RatesReport = ROptions
  type Parameters RatesReport = RParams

  reportOptions _ = 
    [Option "f" ["forward-only"] (NoArg RForwardOnly) "Show only rates from currency 1 to currency 2",
     Option "R" ["rategroup"] (ReqArg (Common . CRateGroup) "GROUP") "Show rates from specified group only",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]

  defaultOptions _ = []

  reportHelp _ = "Show exchange rates history. Parameters:\n" ++
                 "  CCY1: show only rates for this currency." ++
                 "  CCY2: show only rates between CCY1 and CCY2."

  initReport _ options _ = setOutputFormat (commonFlags options)

  runReport _ qry options params =
      showRates qry options params
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

commonFlags :: [ROptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

checkRate True (RParams rFrom rTo) (Ext {getContent = rate}) =
  checkCcy rFrom (rateCurrencyFrom rate) && checkCcy rTo (rateCurrencyTo rate)
checkRate False (RParams rFrom rTo) (Ext {getContent = rate}) =
  (checkCcy rFrom (rateCurrencyFrom rate) && checkCcy rTo (rateCurrencyTo rate)) ||
  (checkCcy rFrom (rateCurrencyTo rate)   && checkCcy rTo (rateCurrencyFrom rate))

checkCcy Nothing _ = True
checkCcy (Just str) ccy =
     cSymbol ccy == str
  || maybe "" show (cIntCode ccy) == str
  || fromMaybe "" (cStrCode ccy) == str

isExplicit (Ext {getContent = Explicit {}}) = True
isExplicit _ = False

showRates qry opts params = do
  let selectRates = case [rgroup | CRateGroup rgroup <- commonFlags opts] of
                      [] -> allRates
                      (r:_) -> getRates r
  shownRates <- gets (selectRates . lsRates)
  let forwardOnly = RForwardOnly `elem` opts
  let rates = filter (checkRate forwardOnly params) $ filter isExplicit $ filter (checkQuery qry) shownRates
      format = case selectOutputFormat (commonFlags opts) of
                 OASCII ASCII -> tableColumns ASCII
                 OCSV csv   -> tableColumns csv
                 OHTML html -> tableColumns html
      showCcy f r = output $ show $ f $ getContent r
      showAmt f x = output $ printf "%0.4f" $ f $ getContent x
  outputText $ unlinesText $
       format $ [([output "DATE"], ALeft, map (prettyPrint . getDate) rates),
                 ([output "GROUP"], ALeft, map (output . getRateGroupName . getAttributes) rates),
                 ([output "CCY 1"], ALeft, map (showCcy rateCurrencyFrom) rates),
                 ([output "CCY 2"], ALeft, map (showCcy rateCurrencyTo) rates),
                 ([output "AMOUNT 1"], ARight, map (showAmt rateAmountFrom) rates),
                 ([output "AMOUNT 2"], ARight, map (showAmt rateAmountTo) rates)]

