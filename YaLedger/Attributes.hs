
module YaLedger.Attributes where

import Data.List
import Text.Regex.PCRE
import qualified Data.Map as M

data AttributeValue =
    Exactly String
  | Optional String
  | OneOf [String]
  | AnyBut String
  | Regexp String
  | Any
  deriving (Eq)

getString :: AttributeValue -> String
getString (Exactly s) = s
getString (Optional s) = s
getString (OneOf ls) = intercalate ", " ls
getString (AnyBut s) = '!':s
getString (Regexp s) = s
getString Any = "*"

instance Show AttributeValue where
  show (Exactly s) = "\"" ++ s ++ "\""
  show (Optional s) = "?\"" ++ s ++ "\""
  show (OneOf lst) = "[" ++ intercalate ", " (map go lst) ++ "]"
    where go s = "\"" ++ s ++ "\""
  show (AnyBut s)  = "!\"" ++ s ++ "\""
  show (Regexp s)  = "/" ++ s ++ "/"
  show Any = "*"

type Attributes = M.Map String AttributeValue

showA :: Attributes -> String
showA attrs = "{" ++ intercalate ", " (map one $ M.assocs attrs) ++ "}"
  where
    one (name, value) = name ++ " = " ++ show value

matchAV :: AttributeValue -> AttributeValue -> Bool
matchAV (Exactly x) (Exactly y) = x == y
matchAV (Exactly x) (AnyBut y)  = x /= y
matchAV (Exactly x) (OneOf lst) = x `elem` lst
matchAV (OneOf l1)  (OneOf l2)  = any (`elem` l2) l1
matchAV (AnyBut x)  (OneOf lst) = all (/= x) lst
matchAV (Exactly x) (Regexp re) = x =~ re
matchAV (OneOf lst) (Regexp re) = any (=~ re) lst
matchAV (Regexp re) (OneOf lst) = any (=~ re) lst
matchAV (Optional x) y          = matchAV (Exactly x) y
matchAV Any         _           = True
matchAV x           y           = matchAV y x

