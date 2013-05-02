{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies, TemplateHaskell #-}

module YaLedger.Reports.Registry
  (Registry (..)) where

import YaLedger.Reports.API
import qualified YaLedger.Reports.Flow as F

data Registry = Registry

data ROptions =
       RInternal (Maybe String)
     | RDot
     | RDescriptions Int
     | Common CommonFlags
  deriving (Eq, Show)

instance ReportClass Registry where
  type Options Registry = ROptions
  type Parameters Registry = Maybe Path

  reportHelp _ = "Show all entries in account or group of accounts."

  reportOptions _ =
    [Option "l" ["ledger"] (NoArg $ Common CLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg $ Common CBothBalances) "Show both available and ledger balances",
     Option "a" ["absolute"] (NoArg $ Common CAbsoluteValues) "Show absolute values of all balances",
     Option "i" ["internal"] (OptArg RInternal "GROUP") "Show only entries where all accounts belong to GROUP",
     Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "d" ["descriptions"] (OptArg rDescriptions "MAXLENGTH") "Show entries descriptions (but do not show rates differences)",
     Option "D" ["dot"] (NoArg RDot) "Output data (only credit amount sums) in DOT format (GraphViz)",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]

  initReport _ options _ = setOutputFormat (commonFlags options)

  runReport _ qry options mbPath = 
      registry qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

commonFlags :: [ROptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

rDescriptions :: Maybe String -> ROptions
rDescriptions Nothing = RDescriptions 15
rDescriptions (Just s) = case reads s of
                          [(n,[])] -> RDescriptions n
                          _ -> error $ "Error while parsing descriptions length: must be integer, but got " ++ s

absBalance extBalance@(Ext {getContent = balance}) =
  extBalance {
    getContent = balance {
                   balanceValue = abs (balanceValue balance)
                 }
             }

registry qry options mbPath = do
    fullCoA <- gets lsCoA
    coa <- getCoAItemL mbPath
    totals <- do
              res <- treeSaldo qry coa
              case res of
                Leaf {..}   -> return leafData
                Branch {..} -> return branchData
    let flags = commonFlags options
    let showCurrs = CNoCurrencies `notElem` flags
    groupInternal <- case [val | RInternal val <- options] of
                       [] -> return Nothing
                       (Nothing:_) -> return $ Just coa
                       (Just grp:_) -> Just <$> getCoAItem (mkPath grp)
    whenJust groupInternal $ \grp ->
        $debug $ "Restricting to internal entries of group:\n" ++ show grp
    case coa of
      Leaf {leafData = account} -> do
          balances <- readIOListL (accountBalances account)
          let mbAbs = if CAbsoluteValues `elem` flags
                        then map absBalance
                        else id
          let balances' = mbAbs $ reverse $ filter (checkBalance coa groupInternal qry) balances
          let bqry = if CBothBalances `elem` flags
                        then BothBalances
                        else if CLedgerBalances `elem` flags
                              then Only LedgerBalance
                              else Only AvailableBalance
          let infoCols = case [k | RDescriptions k <- options] of
                           (l:_) -> [IDescription l]
                           _ -> []
          let format = case selectOutputFormat flags of
                         OASCII _ -> showEntriesBalances' bqry flags infoCols fullCoA ASCII totals
                         OCSV csv -> showEntriesBalances' bqry flags infoCols fullCoA csv totals
                         OHTML html -> showEntriesBalances' bqry flags infoCols fullCoA html totals
          if RDot `elem` options
            then outputString $ unlines $ formatDot fullCoA $ mapMaybe (causedBy . getContent) $ nub $ balances'
            else outputText $ format (nub balances')
      Branch {} -> do
          let accounts = map snd $ leafs coa
          allEntries <- forM accounts getEntries
          let entries = reverse $ concat $ map (filter $ checkExtEntry coa groupInternal qry) allEntries
          let infoCols = case [k | RDescriptions k <- options] of
                           (l:_) -> [IDescription l]
                           _ -> [IRatesDifference]
          let format = case selectOutputFormat flags of
                         OASCII _ -> showEntries' fullCoA ASCII   totals flags infoCols 
                         OCSV csv -> showEntries' fullCoA csv totals flags infoCols
                         OHTML html -> showEntries' fullCoA html totals flags infoCols
          if RDot `elem` options
            then outputString $ unlines $ formatDot fullCoA $ map getContent $ nub $ sort $ entries
            else outputText $ format (nub $ sort $ entries)

formatDot :: ChartOfAccounts -> [Entry Decimal Checked] -> [String]
formatDot coa entries =
    F.formatDot sum decimalMantissa coa $ F.flatMap $ F.groupEntries coa entries

