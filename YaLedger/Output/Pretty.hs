{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, RecordWildCards #-}
module YaLedger.Output.Pretty where

import Data.Dates
import Data.List
import Data.Decimal
import Data.String (fromString)
import qualified Data.Map as M
import Text.Printf

import YaLedger.Types
import YaLedger.Output.Formatted

class Pretty a where
  prettyPrint :: a -> FormattedText

pPrint :: Pretty a => a -> String
pPrint x = toString $ prettyPrint x

instance Pretty String where
  prettyPrint s = fromString s

instance Pretty a => Pretty (Ext a) where
  prettyPrint (Ext date _ _ attrs a) =
      fromString (printf "@ %s %s%s%s\n%s"
             (toString $ prettyPrint date)
             (maybe "" (\s -> getString s ++ " ") $ M.lookup "status" attrs)
             (maybe "" (\s -> "(" ++ getString s ++ ") ") $ M.lookup "category" attrs)
             (maybe "" getString $ M.lookup "description" attrs)
             (toString $ prettyPrint attrs)) ++ prettyPrint a

instance Pretty Decimal where
  prettyPrint x =
    let str = show (roundTo 4 x)
    in  if x < 0
          then [Fragment (color Red) str]
          else if x == 0
                 then [Fragment faint str]
                 else fromString str

instance Pretty Attributes where
  prettyPrint as = go $ M.filterWithKey (\name _ -> name `notElem` ["category", "description","status"]) as
    where
      go x
        | M.null x = emptyText
        | otherwise = showA x <> newline

showDate :: DateTime -> String
showDate (DateTime {..}) = printf "%4d/%02d/%02d" year month day

instance Pretty DateTime where
  prettyPrint (DateTime {..}) = fromString $ 
    if (hour == 0) && (minute == 0) && (second == 0)
      then printf "%4d/%02d/%02d" year month day
      else printf "%4d/%02d/%02d, %02d:%02d:%02d" year month day hour minute second

instance Pretty Record where
  prettyPrint (Template name tran) = 
    "template " <> name <> prettyPrint tran
  prettyPrint (RuleR name (Condition {..}) tran) =
    "rule " <> name <> prettyPrint cAction <> " = when " <>
           (intercalate ", " $ map show cAccounts ++ map show cGroups) <>
           space <>
           (prettyPrint cAction) <>
           (prettyPrint cValue) <>
           (if M.null cAttributes
              then emptyText
              else " with " <> showA cAttributes) <>
           " do\n" <> (prettyPrint tran)

  prettyPrint (Periodic name interval tran) =
    "periodic " <> name <>
    " = every " <> prettyPrint interval <>
    " do\n" <> prettyPrint tran
  prettyPrint (StopPeriodic name) =
    "stop " <> name <> newline
  prettyPrint (SetRate rates) = fromString $ unlines (map go rates)
    where
      go (Explicit c1 a1 c2 a2 r) = fromString $
        printf "rate %.04f%s %s %.04f%s"
          a1 (show c1)
          (if r then "<->" else "->")
          a2 (show c2)
      go (Implicit c1 c2 base r) = fromString $
        printf "rate %s %s %s = via %s"
          (show c1)
          (if r then "<->" else "->")
          (show c2)
          (show base)
  prettyPrint (Transaction tran) = prettyPrint tran

instance Pretty DateInterval where
  prettyPrint (Days n)   = show n <> " days"
  prettyPrint (Weeks n)  = show n <> " weeks"
  prettyPrint (Months n) = show n <> " months"
  prettyPrint (Years n)  = show n <> " years"

instance Pretty (Maybe PostingType) where
  prettyPrint Nothing = fromString "use"
  prettyPrint (Just t) = prettyPrint t

instance Pretty PostingType where
  prettyPrint ECredit = fromString "credit"
  prettyPrint EDebit  = fromString "debit"

instance Pretty ValueCondition where
  prettyPrint AnyValue = emptyText
  prettyPrint (MoreThan x) = " > " <> prettyPrint x
  prettyPrint (LessThan x) = " < " <> prettyPrint x
  prettyPrint (Equals x)   = " == " <> prettyPrint x

instance Pretty Amount where
  prettyPrint amt@(x :# _) =
    let str = show amt
    in  if x < 0
          then [Fragment (color Red) str]
          else if x == 0
                 then [Fragment faint str]
                 else fromString str

instance (Pretty a, Eq a) => Pretty (BalanceInfo a) where
  prettyPrint (BalanceInfo Nothing Nothing) = output "NA"
  prettyPrint (BalanceInfo (Just x) Nothing) = prettyPrint x
  prettyPrint (BalanceInfo Nothing (Just x)) = prettyPrint x
  prettyPrint (BalanceInfo (Just a) (Just l))
      | a == l = prettyPrint a
      | otherwise = prettyPrint a <> output " / " <> prettyPrint l

instance Pretty Param where
  prettyPrint (Fixed x) = prettyPrint x
  prettyPrint (Param i c d) =
    output (printf "#%d*%0.2f (default " i c :: String) <>
           prettyPrint d <>
           ")"
  prettyPrint (Plus x y) =
    prettyPrint x <> " + " <> prettyPrint y

instance (Pretty t) => Pretty (Transaction t) where
  prettyPrint (TEntry e) = prettyPrint e
  prettyPrint (TReconciliate btype acc x tgt msg) =
    "reconciliate " <> getName acc <> space <>
    (if btype == AvailableBalance then "" else "ledger ") <>
    (prettyPrint x) <>
    (maybe "" ((" "++) . toString . prettyPrint) msg) <>
    (maybe "\n" (getName) tgt)
  prettyPrint (TCallTemplate name args) =
    "call " <> name <> space <>
           (unwordsText $ map prettyPrint args)
  prettyPrint (THold crholds drholds) = unlinesText $ map prettyPrint crholds ++ map prettyPrint drholds
  prettyPrint (TCloseHolds crholds drholds) = unlinesText $ map go crholds ++ map go drholds
      where go clh = "close " <>
                     (if searchLesserAmount clh then "<= " else "") <>
                     prettyPrint (holdPosting $ holdToClose clh)

instance Pretty v => Pretty (Hold v t) where
  prettyPrint (Hold p _) = "hold" <> prettyPrint p

instance Pretty ReconciliationMessage where
  prettyPrint (RWarning msg) = "warning: " <> prettyPrint msg
  prettyPrint (RError   msg) = "error: " <> prettyPrint msg

instance Pretty MessageFormat where
  prettyPrint list = concatMap prettyPrint list

instance Pretty MessageElement where
  prettyPrint (MVariable name) = "#" <> name
  prettyPrint (MFixed str) = fromString str

instance (Pretty v) => Pretty (Entry v c) where
  prettyPrint (CEntry dt cr _) =
    unlinesText $ map prettyPrint dt ++ map prettyPrint cr
  prettyPrint (UEntry dt cr corr _) =
    (unlinesText $ map prettyPrint dt ++ map prettyPrint cr) <>
    (case corr of
      Nothing -> emptyText
      Just (acc, useHold) -> spaces 2 <> prettyPrint useHold <> getName acc <> newline)

instance Pretty HoldUsage where
  prettyPrint DontUseHold = fromString ""
  prettyPrint TryUseHold  = fromString "try use "
  prettyPrint UseHold     = fromString "use "

instance (Pretty v) => Pretty (Posting v t) where
  prettyPrint (DPosting acc x DontUseHold) =
    "  dr " <> getName acc <> prettyPrint x
  prettyPrint (CPosting acc x DontUseHold) =
    "  cr " <> getName acc <> prettyPrint x
  prettyPrint (DPosting acc x TryUseHold) =
    "try use dr " <> getName acc <> prettyPrint x
  prettyPrint (CPosting acc x TryUseHold) =
    "try use cr " <> getName acc <> prettyPrint x
  prettyPrint (DPosting acc x UseHold) =
    "use dr " <> getName acc <> prettyPrint x
  prettyPrint (CPosting acc x UseHold) =
    "use cr " <> getName acc <> prettyPrint x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPrint (Left x) = prettyPrint x
  prettyPrint (Right x) = prettyPrint x

