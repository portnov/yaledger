{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies, TemplateHaskell #-}

module YaLedger.Reports.IncomeStatement where

import YaLedger.Reports.API

data IncomeStatement = IncomeStatement

data IOptions = Common CommonFlags
              | IIncomesCls String
              | IExpencesCls String
              | IIncomesOnly
              | IExpencesOnly
  deriving (Eq)

instance ReportClass IncomeStatement where
  type Options IncomeStatement = IOptions
  type Parameters IncomeStatement = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg $ Common CNoZeros) "Do not show accounts with zero balance",
     Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "I" ["incomes-only"] (NoArg IIncomesOnly) "Show incomes only",
     Option "E" ["expences-only"] (NoArg IExpencesOnly) "Show expences only",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry opts mbPath = 
      incomeStatement' qry opts mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

commonFlags :: [IOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

anyAccount :: (AnyAccount -> Bool) -> ChartOfAccounts -> Bool
anyAccount fn coa = not $ isEmptyTree $ filterLeafs fn coa 

incomeStatement' qry options mbPath = do
    opts <- gets lsConfig
    coa <- getCoAItemL mbPath
    let flags = commonFlags options
    
    $debug $ "Use incomes query: " ++ showA (incomeAccounts opts)
    $debug $ "Use expences query: " ++ showA (expenceAccounts opts)

    let isIncomesAcc  acc = isIncomes  opts (accountAttributes acc)
        isExpencesAcc acc = isExpences opts (accountAttributes acc)

    let useIncomesQry  = anyAccount isIncomesAcc  coa
        useExpencesQry = anyAccount isExpencesAcc coa

    let isCredit (WCredit _) = True
        isCredit _           = False

        isDebit (WDebit _) = True
        isDebit _          = False

        incomesCheck
          | useIncomesQry = isIncomesAcc
          | otherwise     = isDebit

        expencesCheck
          | useExpencesQry = isExpencesAcc
          | otherwise      = isCredit

        amount (Branch {..}) = branchData
        amount (Leaf   {..}) = leafData

        incomes  = filterLeafs incomesCheck  coa
        expences = filterLeafs expencesCheck coa

        nz x = if CNoZeros `elem` flags
                 then isNotZero x
                 else True

    let prepareIncomes
          | anyAccount (isAssets opts . accountAttributes) coa = id
          | otherwise = mapTree negateAmount negateAmount 

    let hideCcys = CNoCurrencies `elem` commonFlags options

    let outputASCII incomes expences = do
        let defcur = getCurrency (amount incomes)
        incomeD  :# _ <- convert (qEnd qry) defcur (amount incomes)
        outcomeD :# _ <- convert (qEnd qry) defcur (amount expences)
        let incomesS  = map toString $ showTree $ filterLeafs nz incomes
            expencesS = map toString $ showTree $ filterLeafs nz expences
            m = max (length incomesS) (length expencesS)
            padE list = list ++ replicate (m - length list) ""
            res = twoColumns (output "INCOMES") (output "EXPENCES")
                     (alignMax ALeft $ map output $ padE incomesS)
                     (alignMax ALeft $ map output $ padE expencesS)
            footer = "    TOTALS: " <> prettyPrint (incomeD - outcomeD) <> show defcur
        outputText $ unlinesText (res ++ [footer])
    
    let outputCSV csv incomes expences = do
        when (IExpencesOnly `notElem` options) $ do
            outputText $ output "INCOMES"
            let incomePaths = map output $ map (intercalate "/") $ allLeafPaths incomes
                incomesS = map (printAmt hideCcys) $ allLeafs incomes
            outputText $ unlinesText $
              tableColumns csv $
                [([output "ACCOUNT"], ALeft, incomePaths),
                 ([output "AMOUNT"], ARight, incomesS)]
        when (IIncomesOnly `notElem` options) $ do
            outputText $ output "EXPENCES"
            let expencePaths = map output $ map (intercalate "/") $ allLeafPaths expences
                expencesS = map (printAmt hideCcys) $ allLeafs expences
            outputText $ unlinesText $
              tableColumns csv $
                [([output "ACCOUNT"], ALeft, expencePaths),
                 ([output "AMOUNT"], ARight, expencesS)]

    incomes'  <- prepareIncomes <$> treeSaldo qry incomes
    expences' <- treeSaldo qry expences

    case selectOutputFormat flags of
      OASCII _ -> outputASCII incomes' expences'
      OCSV csv -> outputCSV csv (filterLeafs nz incomes') (filterLeafs nz expences')

printAmt False amt = prettyPrint amt
printAmt True (x :# _) = prettyPrint x

