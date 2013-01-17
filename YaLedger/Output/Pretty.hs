{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, RecordWildCards #-}
module YaLedger.Output.Pretty where

import Data.Dates
import Data.List
import Data.Decimal
import qualified Data.Map as M
import Text.Printf

import YaLedger.Types

class Pretty a where
  prettyPrint :: a -> String

instance Pretty String where
  prettyPrint s = s

instance Pretty a => Pretty (Ext a) where
  prettyPrint (Ext date _ _ attrs a) =
      printf "@ %s %s%s%s\n%s%s"
             (prettyPrint date)
             (maybe "" (\s -> getString s ++ " ") $ M.lookup "status" attrs)
             (maybe "" (\s -> "(" ++ getString s ++ ") ") $ M.lookup "category" attrs)
             (maybe "" getString $ M.lookup "description" attrs)
             (prettyPrint attrs)
             (prettyPrint a)

instance Pretty Decimal where
  prettyPrint x = show (roundTo 4 x)

instance Pretty Attributes where
  prettyPrint as = go $ M.filterWithKey (\name _ -> name `notElem` ["category", "description","status"]) as
    where
      go x
        | M.null x = ""
        | otherwise = showA x ++ "\n"

showDate :: DateTime -> String
showDate (DateTime {..}) = printf "%4d/%02d/%02d" year month day

instance Pretty DateTime where
  prettyPrint (DateTime {..}) =
    if (hour == 0) && (minute == 0) && (second == 0)
      then printf "%4d/%02d/%02d" year month day
      else printf "%4d/%02d/%02d, %02d:%02d:%02d" year month day hour minute second

instance Pretty Record where
  prettyPrint (Template name tran) = 
    printf "template %s\n%s" name (prettyPrint tran)
  prettyPrint (RuleR name (Condition {..}) tran) =
    printf "rule %s = when %s %s%s%s do\n%s"
           name
           (prettyPrint cAction)
           (intercalate ", " $ map show cAccounts ++ map show cGroups)
           (prettyPrint cValue)
           (if M.null cAttributes
              then ""
              else " with " ++ showA cAttributes)
           (prettyPrint tran)
  prettyPrint (Periodic name interval tran) =
    printf "periodic %s = every %s do\n%s"
           name
           (prettyPrint interval)
           (prettyPrint tran)
  prettyPrint (StopPeriodic name) =
    "stop " ++ name ++ "\n"
  prettyPrint (SetRate rates) = unlines (map go rates)
    where
      go (Explicit c1 a1 c2 a2 r) =
        printf "rate %.04f%s %s %.04f%s"
          a1 (show c1)
          (if r then "<->" else "->")
          a2 (show c2)
      go (Implicit c1 c2 base r) =
        printf "rate %s %s %s = via %s"
          (show c1)
          (if r then "<->" else "->")
          (show c2)
          (show base)
  prettyPrint (Transaction tran) = prettyPrint tran

instance Pretty DateInterval where
  prettyPrint (Days n)   = show n ++ " days"
  prettyPrint (Weeks n)  = show n ++ " weeks"
  prettyPrint (Months n) = show n ++ " months"
  prettyPrint (Years n)  = show n ++ " years"

instance Pretty (Maybe PostingType) where
  prettyPrint Nothing = "use"
  prettyPrint (Just t) = prettyPrint t

instance Pretty PostingType where
  prettyPrint ECredit = "credit"
  prettyPrint EDebit  = "debit"

instance Pretty ValueCondition where
  prettyPrint AnyValue = ""
  prettyPrint (MoreThan x) = " > " ++ prettyPrint x
  prettyPrint (LessThan x) = " < " ++ prettyPrint x
  prettyPrint (Equals x)   = " == " ++ prettyPrint x

instance Pretty Amount where
  prettyPrint x = show x

instance Pretty Param where
  prettyPrint (Fixed x) = prettyPrint x
  prettyPrint (Param i c d) =
    printf "#%d*%0.2f (default %s)"
           i c (prettyPrint d)
  prettyPrint (Plus x y) =
    printf "%s + %s"
           (prettyPrint x)
           (prettyPrint y)

instance (Pretty t) => Pretty (Transaction t) where
  prettyPrint (TEntry e) = prettyPrint e
  prettyPrint (TReconciliate btype acc x tgt msg) =
    printf "reconciliate %s %s%s%s%s" (getName acc)
                                    (if btype == AvailableBalance then "" else "ledger ")
                                    (prettyPrint x)
                                    (maybe "" ((" "++) . prettyPrint) msg)
                                    (maybe "\n" (getName) tgt)
  prettyPrint (TCallTemplate name args) =
    printf "call %s %s\n"
           name
           (unwords $ map prettyPrint args)
  prettyPrint (THold crholds drholds) = unlines $ map prettyPrint crholds ++ map prettyPrint drholds
  prettyPrint (TCloseHolds crholds drholds) = unlines $ map go crholds ++ map go drholds
      where go clh = "close " ++ 
                     (if searchLesserAmount clh then "<= " else "") ++
                     prettyPrint (holdPosting $ holdToClose clh)

instance Pretty v => Pretty (Hold v t) where
  prettyPrint (Hold p _) = "hold" ++ prettyPrint p

instance Pretty ReconciliationMessage where
  prettyPrint (RWarning msg) = "warning: " ++ prettyPrint msg
  prettyPrint (RError   msg) = "error: " ++ prettyPrint msg

instance Pretty MessageFormat where
  prettyPrint list = concatMap prettyPrint list

instance Pretty MessageElement where
  prettyPrint (MVariable name) = '#':name
  prettyPrint (MFixed str) = str

instance (Pretty v) => Pretty (Entry v c) where
  prettyPrint (CEntry dt cr _) =
    unlines $ map prettyPrint dt ++ map prettyPrint cr
  prettyPrint (UEntry dt cr corr _) =
    (unlines $ map prettyPrint dt ++ map prettyPrint cr) ++
    (case corr of
      Nothing -> ""
      Just (acc, useHold) -> "  " ++ prettyPrint useHold ++ getName acc ++ "\n")

instance Pretty HoldUsage where
  prettyPrint DontUseHold = ""
  prettyPrint TryUseHold  = "try use "
  prettyPrint UseHold     = "use "

instance (Pretty v) => Pretty (Posting v t) where
  prettyPrint (DPosting acc x DontUseHold) =
    printf "  dr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (CPosting acc x DontUseHold) =
    printf "  cr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (DPosting acc x TryUseHold) =
    printf "try use dr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (CPosting acc x TryUseHold) =
    printf "try use cr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (DPosting acc x UseHold) =
    printf "use dr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (CPosting acc x UseHold) =
    printf "use cr %s  %s"
           (getName acc)
           (prettyPrint x)

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPrint (Left x) = prettyPrint x
  prettyPrint (Right x) = prettyPrint x

