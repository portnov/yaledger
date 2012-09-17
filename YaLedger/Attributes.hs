
module YaLedger.Attributes where

import Data.List
import Text.Regex.PCRE
import qualified Data.Map as M

import Debug.Trace

data AttributeValue =
    Exactly String
  | AnyBut String
  | Regexp String
  deriving (Eq)

instance Show AttributeValue where
  show (Exactly s) = "\"" ++ s ++ "\""
  show (AnyBut s)  = "!\"" ++ s ++ "\""
  show (Regexp s)  = "/" ++ s ++ "/"

type Attributes = M.Map String AttributeValue

showA :: Attributes -> String
showA attrs = "{" ++ intercalate ", " (map one $ M.assocs attrs) ++ "}"
  where
    one (name, value) = name ++ " = " ++ show value

matchAV :: AttributeValue -> AttributeValue -> Bool
matchAV (Exactly x) (Exactly y) = x == y
matchAV (Exactly x) (AnyBut y)  = x /= y
matchAV (AnyBut x)  (Exactly y) = x /= y
matchAV (Exactly x) (Regexp re) = traceS (x ++ " =~ " ++ re ++ ": ") $ x =~ re
matchAV (Regexp re) (Exactly x) = traceS (x ++ " =~ " ++ re ++ ": ") $ x =~ re
matchAV _           _           = False

traceS :: Show a => String -> a -> a
traceS a x = trace (a ++ show x) x

