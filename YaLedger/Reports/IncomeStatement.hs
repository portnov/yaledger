{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}

module YaLedger.Reports.IncomeStatement where

import YaLedger.Reports.API
import qualified Data.Map as M

data IncomeStatement = IncomeStatement

data IOptions = INoZeros
              | IIncomesCls String
              | IExpencesCls String
  deriving (Eq)

instance ReportClass IncomeStatement where
  type Options IncomeStatement = IOptions
  type Parameters IncomeStatement = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg INoZeros) "Do not show accounts with zero balance" ]
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

isEmptyTree :: Tree n a -> Bool
isEmptyTree (Branch {branchChildren = list}) = null list
isEmptyTree (Leaf {}) = False

anyAccount :: (AnyAccount -> Bool) -> ChartOfAccounts -> Bool
anyAccount fn coa = not $ isEmptyTree $ filterLeafs fn coa 

incomeStatement' qry options mbPath = do
    opts <- gets lsConfig
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    
    debug $ "Use incomes query: " ++ showA (incomeAccounts opts)
    debug $ "Use expences query: " ++ showA (expenceAccounts opts)

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

        nz x = if INoZeros `elem` options
                 then isNotZero x
                 else True

    let prepareIncomes
          | anyAccount (isAssets opts . accountAttributes) coa = id
          | otherwise = mapTree negateAmount negateAmount 

    incomes'  <- prepareIncomes <$> treeSaldo qry incomes
    expences' <- treeSaldo qry expences

    let defcur = getCurrency (amount incomes')
    incomeD  :# _ <- convert (qEnd qry) defcur (amount incomes')
    outcomeD :# _ <- convert (qEnd qry) defcur (amount expences')

    let incomesS  = lines (show $ filterLeafs nz incomes')
        expencesS = lines (show $ filterLeafs nz expences')
        m = max (length incomesS) (length expencesS)
        padE list = list ++ replicate (m - length list) ""
        res = twoColumns "INCOMES" "EXPENCES"
                 (alignMax ALeft $ padE incomesS)
                 (alignMax ALeft $ padE expencesS)
        footer = "    TOTALS: " ++ show (incomeD - outcomeD) ++ show defcur

    wrapIO $ putStrLn $ unlines (res ++ [footer])

