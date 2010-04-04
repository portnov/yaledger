
module TopLevel where

import Text.ParserCombinators.Parsec (runParser)
import qualified Data.Map as M
import Data.Maybe (isJust)

import Types
import Dates
import Parser
import Currencies
import Accounts
import Transactions
import qualified Tree as T

tryParse parser st src str = 
  case runParser parser st src str of
    Right x -> x
    Left e -> error $ "Cannot parse:\n" ++ show e ++ "\n [ERROR]\n"

readLedger :: DateTime -> FilePath -> IO (AccountsTree, [Dated Record])
readLedger dt path = do
  str <- readFile path
  let y = year dt
      (accs, recs) = tryParse (ledgerSource y) emptyPState path str
  return (accs, recs)

runQuery :: DateTime -> [Condition] -> AccountsTree -> [Dated Record] -> LedgerState 
runQuery dt pred accs recs = 
  let st = LS dt accs undefined [] M.empty M.empty [] []
  in  case runState (doRecords pred recs) st of
        Right ((), y) -> y
        Left e        -> error $ show e

balance :: LedgerState -> T.Tree Amount Amount
balance st = calcBalances (rates st) (accounts st)

printBalance :: LedgerState -> IO ()
printBalance st =
  putStrLn $ T.showTree 0 (balance st)

printRegister :: LedgerState -> String -> IO ()
printRegister st path = do
    putStr "Date "
    putStrLn $ unwords $ accs
    putStrLn $ unlines $ map showRec $ filter nonEmpty $ map (\r -> (getDate r, amountsList' r accs)) (records st)
  where
    tree = balance st
    accs = T.lookupNode path tree

    showRec :: (DateTime, [Maybe Amount]) -> String
    showRec (dt, lst) = showDate dt ++ " " ++ (unwords $ map showMaybe lst)

    showMaybe :: Maybe Amount -> String
    showMaybe (Just x) = show $ getValue x
    showMaybe Nothing = "NA"

    nonEmpty :: (DateTime, [Maybe Amount]) -> Bool
    nonEmpty (_,lst) = any isJust lst

accountFromTree :: AccountsTree -> String -> Account
accountFromTree accs path = 
  case T.lookupPath path accs of
    []    -> error $ "Unknown account: " ++ path
    [acc] -> acc
    _     -> error $ "Ambigous account spec: " ++ path

saldo :: DateTime -> [Condition] -> AccountsTree -> [Dated Record] -> String -> String -> String -> Amount
saldo now conds accs recs path start end = 
    let y = year now
        startD = tryParse (pDateOnly y) emptyPState "<start date>" start
        endD   = tryParse (pDateOnly y) emptyPState "<end date>" end
        startSt = runQuery now ((mkDateCondition startD (>=)):conds) accs recs
        endSt   = runQuery now ((mkDateCondition endD   (<=)):conds) accs recs
        accStart = accountFromTree (accounts startSt) path
        accEnd   = accountFromTree (accounts endSt)   path
        startAmount = sumAccount accStart
        endAmount = sumAccount accEnd
    in  amountPlus (rates endSt) startAmount (negateAmount endAmount)

