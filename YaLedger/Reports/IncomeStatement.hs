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
    [Option "z" ["no-zeros"] (NoArg INoZeros) "Do not show accounts with zero balance",
     Option "i" ["incomes"]  (ReqArg IIncomesCls "CLASSIFIER") "Use CLASSIFIER to detect incomes accounts",
     Option "e" ["expences"] (ReqArg IExpencesCls "CLASSIFIER") "Use CLASSIFIER to detect expences accounts" ]
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

matchClassifier :: String -> AnyAccount -> Bool
matchClassifier cls acc =
  case M.lookup "classifier" (accountAttributes acc) of
    Nothing -> False
    Just value -> matchAV (Exactly cls) value

anyAccountHasClassifier :: String -> ChartOfAccounts -> Bool
anyAccountHasClassifier cls coa =
    not $ isEmptyTree $ filterLeafs (matchClassifier cls) coa 

incomeStatement' qry options mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path

    let (incomesClassifier, useIncomesClassifier) =
          case [cls | IIncomesCls cls <- options] of
            []  -> ("incomes", anyAccountHasClassifier "incomes" coa)
            lst -> (last lst, True)

        (expencesClassifier, useExpencesClassifier) =
          case [cls | IExpencesCls cls <- options] of
            []  -> ("expences", anyAccountHasClassifier "expences" coa)
            lst -> (last lst, True)

    let isCredit (WCredit _ _) = True
        isCredit _             = False

        isDebit (WDebit _ _) = True
        isDebit _            = False

        incomesCheck
          | useIncomesClassifier = matchClassifier incomesClassifier
          | otherwise            = isDebit

        expencesCheck
          | useExpencesClassifier = matchClassifier expencesClassifier
          | otherwise             = isCredit

        amount (Branch {..}) = branchData
        amount (Leaf   {..}) = leafData

        incomes  = filterLeafs incomesCheck  coa
        expences = filterLeafs expencesCheck coa

        nz x = if INoZeros `elem` options
                 then isNotZero x
                 else True

    incomes'  <- mapTree negateAmount negateAmount <$> treeSaldo qry incomes
    expences' <- treeSaldo qry expences

    let defcur = getCurrency (amount incomes')
    incomeD  :# _ <- convert (qEnd qry) defcur (amount incomes')
    outcomeD :# _ <- convert (qEnd qry) defcur (amount expences')

    let incomesS  = lines (show $ filterLeafs nz incomes')
        expencesS = lines (show $ filterLeafs nz expences')
        m = max (length incomesS) (length expencesS)
        padE list = list ++ replicate (m - length list) ""
        res = twoColumns "INCOMES" "OUTCOMES"
                 (alignMax ALeft $ padE incomesS)
                 (alignMax ALeft $ padE expencesS)
        footer = "    TOTALS: " ++ show (incomeD - outcomeD) ++ show defcur

    wrapIO $ putStrLn $ unlines (res ++ [footer])

