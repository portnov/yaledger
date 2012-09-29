
module YaLedger.Attributes where

import Data.List
import Text.Regex.PCRE
import qualified Data.Map as M

import Debug.Trace

data AttributeValue =
    Exactly String
  | OneOf [String]
  | AnyBut String
  | Regexp String
  | Any
  deriving (Eq)

getString :: AttributeValue -> String
getString (Exactly s) = s
getString (OneOf ls) = intercalate ", " ls
getString (AnyBut s) = '!':s
getString (Regexp s) = s
getString Any = "*"

instance Show AttributeValue where
  show (Exactly s) = "\"" ++ s ++ "\""
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
matchAV (OneOf lst) (Exactly x) = x `elem` lst
matchAV (OneOf l1)  (OneOf l2)  = any (`elem` l2) l1
matchAV (AnyBut x)  (OneOf lst) = all (/= x) lst
matchAV (OneOf lst) (AnyBut x)  = all (/= x) lst
matchAV (AnyBut x)  (Exactly y) = x /= y
matchAV (Exactly x) (Regexp re) = x =~ re
matchAV (Regexp re) (Exactly x) = x =~ re
matchAV (OneOf lst) (Regexp re) = any (=~ re) lst
matchAV (Regexp re) (OneOf lst) = any (=~ re) lst
matchAV _           Any         = True
matchAV Any         _           = True
matchAV _           _           = False

traceS :: Show a => String -> a -> a
traceS a x = trace (a ++ show x) x

