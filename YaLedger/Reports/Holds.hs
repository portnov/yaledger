{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, GADTs #-}

module YaLedger.Reports.Holds
  (Holds (..)) where

import YaLedger.Reports.API

data Holds = Holds

data HOptions =
    HOpenOnly
  | HCSV (Maybe String)
  deriving (Eq)

instance ReportClass Holds where
  type Options Holds = HOptions
  type Parameters Holds = Maybe Path

  reportOptions _ = 
    [Option "o" ["open-only"] (NoArg HOpenOnly) "Show open holds only",
     Option "C" ["csv"] (OptArg HCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]

  runReport _ qry options path = showHolds qry options path

  reportHelp _ = "Show account holds. One optional parameter: account or accounts group."

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

showSign (CPosting {}) = "CR"
showSign (DPosting {}) = "DR"

holdsTable coa fmt holds =
  let showH = either (showHold coa fmt) (showHold coa fmt)
      list = map showH holds
  in  unlines $
      tableGrid fmt [(ALeft, ["DATE"]),
                     (ACenter, ["SIGN"]),
                     (ALeft, ["ACCOUNT"]),
                     (ARight, ["AMOUNT"]),
                     (ALeft, ["CLOSE"]) ] list

showHolds' qry options account = do
  fullCoA <- gets lsCoA
  allHolds <- getHoldsHistory qry account

  let checkEnd :: Ext (Hold Decimal t) -> Bool
      checkEnd extHold = isHoldOpen Nothing (qEnd qry) extHold

      checkHold
        | HOpenOnly `elem` options = either checkEnd checkEnd
        | otherwise = const True

  let holds = sort $ filter checkHold allHolds

  let format = case [s | HCSV s <- options] of
                 []    -> holdsTable fullCoA ASCII
                 (x:_) -> holdsTable fullCoA (CSV x)

  when (not $ null holds) $ do
    let Just path = accountFullPath (getID account) fullCoA
    wrapIO $ do
        putStrLn $ intercalate "/" path ++ ":"
        putStrLn $ format holds

