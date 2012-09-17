{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, RecordWildCards #-}
module YaLedger.Pretty where

import Data.Maybe
import Data.Dates
import qualified Data.Map as M
import Text.Printf

import YaLedger.Types

class Pretty a where
  prettyPrint :: a -> String

instance Pretty String where
  prettyPrint s = s

instance Pretty a => Pretty (Ext a) where
  prettyPrint (Ext date attrs a) =
      printf "@ %s %s\n%s%s"
             (prettyPrint date)
             (maybe "" show $ M.lookup "description" attrs)
             (prettyPrint attrs)
             (prettyPrint a)

instance Pretty Attributes where
  prettyPrint as = go $ M.filterWithKey (\name _ -> name /= "description") as
    where
      go x
        | M.null x = ""
        | otherwise = showA x ++ "\n"

instance Pretty DateTime where
  prettyPrint (DateTime {..}) = printf "%4d/%02d/%02d" year month day

instance Pretty Record where
  prettyPrint (Template name tran) = 
    printf "template %s\n%s" name (prettyPrint tran)
  prettyPrint (Transaction tran) = prettyPrint tran

instance Pretty Amount where
  prettyPrint (x :# c) = show x ++ c

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
  prettyPrint (TReconciliate acc x) =
    printf "reconciliate %s %s\n" (getName acc) (prettyPrint x)
  prettyPrint (TCallTemplate name args) =
    printf "call %s %s\n"
           name
           (unwords $ map prettyPrint args)
  prettyPrint (TSetRate c1 c2 x) =
    printf "rate %s -> %s = %0.4f\n" c1 c2 x

instance (Pretty v) => Pretty (Entry v c) where
  prettyPrint (CEntry dt cr _) =
    unlines $ map prettyPrint dt ++ map prettyPrint cr
  prettyPrint (UEntry dt cr corr _) =
    (unlines $ map prettyPrint dt ++ map prettyPrint cr) ++
    (case corr of
      Nothing -> ""
      Just acc -> "  " ++ getName acc ++ "\n")

instance (Pretty v) => Pretty (Posting v t) where
  prettyPrint (DPosting acc x) =
    printf "  dr %s  %s"
           (getName acc)
           (prettyPrint x)
  prettyPrint (CPosting acc x) =
    printf "  cr %s  %s"
           (getName acc)
           (prettyPrint x)

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPrint (Left x) = prettyPrint x
  prettyPrint (Right x) = prettyPrint x

