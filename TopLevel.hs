{-# LANGUAGE UnicodeSyntax #-}
module TopLevel where

import Text.ParserCombinators.Parsec (runParser)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Control.Monad.Identity

import Types
import Unicode
import Dates
import Parser
import Currencies
import Accounts
import Transactions
import qualified Tree as T

-- import Debug.Trace

trace ∷ String → a → a
trace x y = y

tryParse parser st src str = 
  case runParser parser st src str of
    Right x → x
    Left e → error □ "Cannot parse:\n" ⧺ show e ⧺ "\n [ERROR]\n"

readLedger ∷ DateTime → FilePath → IO (AccountsTree, [Dated Record])
readLedger dt path = do
  str ← readFile path
  let y = year dt
      (accs, recs) = tryParse ledgerSource (emptyPState dt) path str
  return (accs, recs)

runQuery ∷ DateTime → Conditions → AccountsTree → [Dated Record] → LedgerState 
runQuery dt pred accs recs = 
  let st = LS dt accs undefined [] M.empty M.empty [] []
  in  case runState (doRecords (trace ("C: "⧺show pred) pred) recs) st of
        Right ((), y) → y
        Left e        → error □ show e

balance ∷ LedgerState → T.Tree Amount ABalance
balance st = calcBalances (rates st) (accounts st)

printBalance ∷ LedgerState → IO ()
printBalance st =
  putStrLn □ T.showTree 0 (balance st)

printRegister ∷ LedgerState → String → IO ()
printRegister st path = do
    putStr "Date "
    putStrLn □ unwords □ accs
    putStrLn □ unlines □ map showRec □ filter nonEmpty □ map (\r → (getDate r, amountsList' r accs)) (records st)
  where
    tree = balance st
    accs = concatMap T.leafNames □ T.lookupNode path tree

    showRec ∷ (DateTime, [Maybe Amount]) → String
    showRec (dt, lst) = showDate dt ⧺ " " ⧺ (unwords □ map showMaybe lst)

    showMaybe ∷ Maybe Amount → String
    showMaybe (Just x) = show □ getValue x
    showMaybe Nothing = "NA"

    nonEmpty ∷ (DateTime, [Maybe Amount]) → Bool
    nonEmpty (_,lst) = any isJust lst

getAccountSaldo ∷ LedgerState → DateTime → String → String → String → Double
getAccountSaldo st now path startS endS =
  let start = tryParse pDateOnly (emptyPState now) "<start date>" startS
      end   = tryParse pDateOnly (emptyPState now) "<end date>" endS
      acc   = runIdentity □ accountFromTree (accounts st) path
  in  saldo acc start end

-- getGroupSaldo ∷ LedgerState → DateTime → String → String → String → Double
-- getGroupSaldo st now path startS endS = 
--   let start = tryParse pDateOnly (emptyPState now) "<start date>" startS
--       end   = tryParse pDateOnly (emptyPState now) "<end date>" endS
--       grp = runIdentity □ groupFromTree (accounts st) path
