{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses #-}

module YaLedger.Reports.Ledger where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Data.Maybe
import qualified Data.Map as M
import Data.Decimal
import Data.String

import YaLedger.Types
import YaLedger.Output
import YaLedger.Kernel
import YaLedger.Exceptions
import YaLedger.Reports.Common

data InfoColumn =
       IRatesDifference
     | IDescription Int
  deriving (Eq,Show)

-- | Show amount: show currency only if there is no CNoCurrencies flag in options.
showAmt :: [CommonFlags] -> Amount -> FormattedText
showAmt options a@(x :# c)
  | CNoCurrencies `elem` options = prettyPrint x
  | otherwise = prettyPrint a

-- | Show BalanceInfo: show currency only if there is no CNoCurrencies flag in options.
showBI :: [CommonFlags] -> BalanceInfo Amount -> FormattedText
showBI _ (BalanceInfo Nothing Nothing) = output "NA"
showBI options (BalanceInfo (Just x) Nothing) = showAmt options x
showBI options (BalanceInfo Nothing (Just x)) = showAmt options x
showBI options (BalanceInfo (Just a) (Just l))
    | a == l = showAmt options a
    | otherwise = showAmt options a <> output " / " <> showAmt options l

-- | Show BalanceInfo: show currency only if there is no CNoCurrencies flag in options.
showBI' :: [CommonFlags] -> BalanceInfo Decimal -> Currency -> FormattedText
showBI' _ (BalanceInfo Nothing Nothing) _ = output "NA"
showBI' options (BalanceInfo (Just x) Nothing) c = showAmt options (x :# c)
showBI' options (BalanceInfo Nothing (Just x)) c = showAmt options (x :# c)
showBI' options (BalanceInfo (Just a) (Just l)) c
    | a == l = showAmt options (a :# c)
    | otherwise = showAmt options (a :# c) <> output " / " <> showAmt options (l :# c)

showPostingAccount :: Maybe Int -> ChartOfAccounts -> Posting v t -> FormattedText
showPostingAccount t coa (CPosting acc _ _) = output $ maybe "" (trimPath t) $ accountFullPath (getID acc) coa
showPostingAccount t coa (DPosting acc _ _) = output $ maybe "" (trimPath t) $ accountFullPath (getID acc) coa

showPostingValueD :: Bool -> Posting Decimal t -> FormattedText
showPostingValueD True (CPosting acc x _) = prettyPrint (x :# getCurrency acc)
showPostingValueD True (DPosting acc x _) = prettyPrint (x :# getCurrency acc)
showPostingValueD False (CPosting _ x _) = prettyPrint x
showPostingValueD False (DPosting _ x _) = prettyPrint x

showPostingValue :: Posting Amount t -> FormattedText
showPostingValue (CPosting _ x _) = prettyPrint x
showPostingValue (DPosting _ x _) = prettyPrint x

showE :: Ext (Entry Decimal Checked) -> Row
showE (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) emptyText,
     map posting dt, map posting cr, rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [output $ show rd]

showE' :: Maybe Int -> [CommonFlags] -> [InfoColumn] -> ChartOfAccounts -> Ext (Entry Decimal Checked) -> Row
showE' t flags infos coa (Ext {getDate = date, getContent = (CEntry dt cr rd), getAttributes = attrs}) =
    [prettyPrint date: replicate (m-1) emptyText,
     map (showPostingAccount t coa) dt, map (showPostingValueD showCurrs) dt,
     map (showPostingAccount t coa) cr, map (showPostingValueD showCurrs) cr] ++
    if IRatesDifference `elem` infos
      then [rdS]
      else [] ++
    case [k | IDescription k <- infos] of
      (l:_) -> [[output $ takeS l description]]
      _ -> []
  where
    m = max (length cr) (length dt)
    showCurrs = CNoCurrencies `notElem` flags

    rdS
      | rd == OneCurrency = []
      | otherwise = [output $ show rd]

    description = maybe "" getString $ M.lookup "description" attrs

showB :: Currency -> Ext (Balance Checked) -> Row
showB currency (Ext {getDate = date, getContent = balance}) =
  let bd = balanceValue balance
      dt :: [Posting Decimal Debit]
      cr :: [Posting Decimal Credit]
      (dt, cr) = case causedBy balance of
                   Nothing -> ([], [])
                   Just (CEntry dt cr _) -> (dt, cr)
      m = max (length cr) (length dt)
      padding = replicate (m-1) emptyText
  in  [prettyPrint date: padding,
       map posting dt, map posting cr,
       padding ++ [prettyPrint (bd :# currency)]]

takeS :: Int -> String -> String
takeS n str = if length str <= n
                then str
                else take n str ++ "…"

showB' :: Maybe Int -> [CommonFlags] -> [InfoColumn] -> BalanceQuery -> ChartOfAccounts -> Currency -> Ext (Balance Checked) -> Row
showB' t flags infos bqry coa currency (Ext {getDate = date, getContent = balance, getAttributes = attrs}) =
  let bi = getBalanceInfo bqry balance
      dt :: [Posting Decimal Debit]
      cr :: [Posting Decimal Credit]
      (dt, cr) = case causedBy balance of
                   Nothing -> ([], [])
                   Just (CEntry dt cr _) -> (dt, cr)
      m = max (length cr) (length dt)
      padding = replicate (m-1) emptyText
      description = maybe "" getString $ M.lookup "description" attrs
      showCurrs = CNoCurrencies `notElem` flags

  in  [prettyPrint date: padding,
       map (showPostingAccount t coa) dt, map (showPostingValueD showCurrs) dt,
       map (showPostingAccount t coa) cr, map (showPostingValueD showCurrs) cr,
       padding ++ [showBI' flags bi currency]] ++
       case [k | IDescription k <- infos] of
         (l:_) -> [output (takeS l description): padding]
         _ -> []

posting :: Posting Decimal t -> FormattedText
posting (DPosting acc x _) = getName acc <> output ": " <> prettyPrint (x :# getCurrency acc)
posting (CPosting acc x _) = getName acc <> output ": " <> prettyPrint (x :# getCurrency acc)

showEntries :: (TableFormat a) => a -> Amount -> [Ext (Entry Decimal Checked)] -> FormattedText
showEntries fmt totals list =
  let l = map showE list
      footer = showFooter fmt $ "    TOTALS: " <> show totals
  in  unlinesText $
      tableGrid fmt [(ALeft,  [output "DATE"]),
                     (ARight, [output "DEBIT"]),
                     (ARight, [output "CREDIT"]),
                     (ARight, [output "RATES DIFF."])] l ++ footer

showEntries' :: (TableFormat a) => ChartOfAccounts -> a -> Amount -> [CommonFlags] -> [InfoColumn] -> [Ext (Entry Decimal Checked)] -> FormattedText
showEntries' coa fmt totals flags infos list =
  let l = map (showE' (maxFieldWidth fmt) flags infos coa) list
      footer = showFooter fmt $ "    TOTALS: " <> show totals
  in  unlinesText $
      tableGrid fmt ([(ALeft,  [output "DATE"]),
                     (ALeft,  [output "DEBIT ACCOUNT"]),
                     (ARight, [output "AMOUNT"]),
                     (ALeft,  [output "CREDIT ACCOUNT"]),
                     (ARight, [output "AMOUNT"])] ++
                     if IRatesDifference `elem` infos
                       then [ (ARight, [output "RATES DIFF."]) ]
                       else [] ++
                     case [k | IDescription k <- infos] of
                       [] -> []
                       _ -> [(ALeft, [output "DESCRIPTION"])] ) l ++ footer

showEntriesBalances :: (TableFormat a) => a -> Amount -> [Ext (Balance Checked)] -> FormattedText
showEntriesBalances fmt totals list =
  let l = map (showB $ getCurrency totals) list
      footer = showFooter fmt $ "    TOTALS: " <> show totals
  in  unlinesText $
      tableGrid fmt [(ALeft,  [output "DATE"]),
                     (ARight, [output "DEBIT"]),
                     (ARight, [output "CREDIT"]),
                     (ARight, [output "BALANCE B/D"])] l ++ footer

showEntriesBalances' :: (TableFormat a) => BalanceQuery -> [CommonFlags] -> [InfoColumn] -> ChartOfAccounts -> a -> Amount -> [Ext (Balance Checked)] -> FormattedText
showEntriesBalances' bqry flags infos coa fmt totals list =
  let l = map (showB' (maxFieldWidth fmt) flags infos bqry coa (getCurrency totals)) list
      footer = showFooter fmt $ "    TOTALS: " <> show totals
  in  unlinesText $
      tableGrid fmt ([(ALeft,  [output "DATE"]),
                     (ALeft,  [output "DEBIT ACCOUNT"]),
                     (ARight, [output "AMOUNT"]),
                     (ALeft,  [output "CREDIT ACCOUNT"]),
                     (ARight, [output "AMOUNT"]),
                     (ARight, [output "BALANCE B/D"])] ++
                     case [k | IDescription k <- infos] of
                       [] -> []
                       _ -> [(ALeft, [output "DESCRIPTION"])] ) l ++ footer

causedByExt :: Ext (Balance Checked) -> Maybe (Ext (Entry Decimal Checked))
causedByExt (Ext date i pos attrs p) =
  Ext date i pos attrs <$> causedBy p

getEntries :: (Throws InternalError l,
               HasBalances a)
           => a
           -> Ledger l [Ext (Entry Decimal Checked)]
getEntries acc = do
  balances <- readIOListL (accountBalances acc)
  return $ mapMaybe causedByExt balances

