{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, GADTs #-}

module YaLedger.Reports.Holds
  (Holds (..)) where

import YaLedger.Reports.API

data Holds = Holds

data HOptions =
    HOpenOnly
  | Common CommonFlags
  deriving (Eq)

instance ReportClass Holds where
  type Options Holds = HOptions
  type Parameters Holds = Maybe Path

  reportOptions _ = 
    [Option "o" ["open-only"] (NoArg HOpenOnly) "Show open holds only",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]

  initReport _ options _ = setOutputFormat (commonFlags options)

  runReport _ qry options path = showHolds qry options path

  reportHelp _ = "Show account holds. One optional parameter: account or accounts group."

commonFlags :: [HOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

showHolds qry options mbPath = do
      coa <- getCoAItemL mbPath
      let accounts = map snd $ leafs coa
      forM_ accounts $ \account ->
          showHolds' qry options account
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

showHold :: TableFormat fmt => ChartOfAccounts -> fmt -> Ext (Hold Decimal t) -> Row
showHold coa fmt (Ext {getDate = date, getContent = (Hold posting cld)}) = 
  [[prettyPrint date],
   [showSign posting],
   [showPostingAccount (maxFieldWidth fmt) coa posting],
   [prettyPrint (getAmount posting)],
   [showMaybeDate cld]]

showSign (CPosting {}) = output "CR"
showSign (DPosting {}) = output "DR"

holdsTable coa fmt holds =
  let showH = either (showHold coa fmt) (showHold coa fmt)
      list = map showH holds
  in  unlinesText $
      tableGrid fmt [(ALeft,   [output "DATE"]),
                     (ACenter, [output "SIGN"]),
                     (ALeft,   [output "ACCOUNT"]),
                     (ARight,  [output "AMOUNT"]),
                     (ALeft,   [output "CLOSE"]) ] list

showHolds' qry options account = do
  fullCoA <- gets lsCoA
  allHolds <- getHoldsHistory qry account

  let checkEnd :: Ext (Hold Decimal t) -> Bool
      checkEnd extHold = isHoldOpen Nothing (qEnd qry) extHold

      checkHold
        | HOpenOnly `elem` options = either checkEnd checkEnd
        | otherwise = const True

  let holds = sort $ filter checkHold allHolds

  let format = case selectOutputFormat (commonFlags options) of
                 OASCII _    -> holdsTable fullCoA ASCII
                 OCSV csv -> holdsTable fullCoA csv
                 OHTML html -> holdsTable fullCoA html

  when (not $ null holds) $ do
    let Just path = accountFullPath (getID account) fullCoA
    outputString $ intercalate "/" path ++ ":"
    outputText $ format holds

