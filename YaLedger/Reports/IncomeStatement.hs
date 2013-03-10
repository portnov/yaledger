{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}

module YaLedger.Reports.IncomeStatement where

import YaLedger.Reports.API

data IncomeStatement = IncomeStatement

data IOptions = INoZeros
  deriving (Eq)

instance ReportClass IncomeStatement where
  type Options IncomeStatement = IOptions
  type Parameters IncomeStatement = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg INoZeros) "Do not show accounts with zero balance"]
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

incomeStatement' qry options mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    let isCredit (WCredit _ _) = True
        isCredit _             = False

        isDebit (WDebit _ _) = True
        isDebit _            = False

        amount (Branch {..}) = branchData
        amount (Leaf   {..}) = leafData

        incomes  = filterLeafs isDebit  coa
        outcomes = filterLeafs isCredit coa

        nz x = if INoZeros `elem` options
                 then isNotZero x
                 else True

    incomes'  <- mapTree negateAmount negateAmount <$> treeSaldo qry incomes
    outcomes' <- treeSaldo qry outcomes

    let defcur = getCurrency (amount incomes')
    incomeD  :# _ <- convert (qEnd qry) defcur (amount incomes')
    outcomeD :# _ <- convert (qEnd qry) defcur (amount outcomes')

    let incomesS  = lines (show $ filterLeafs nz incomes')
        outcomesS = lines (show $ filterLeafs nz outcomes')
        m = max (length incomesS) (length outcomesS)
        padE list = list ++ replicate (m - length list) ""
        res = twoColumns "INCOMES" "OUTCOMES"
                 (alignMax ALeft $ padE incomesS)
                 (alignMax ALeft $ padE outcomesS)
        sep = replicate (length $ head res) '═'
        footer = "    TOTALS: " ++ show (incomeD - outcomeD) ++ show defcur

    wrapIO $ putStrLn $ unlines (res ++ [sep, footer])

