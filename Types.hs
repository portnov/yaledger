{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, ExistentialQuantification #-}

module Types where

import Control.Monad.State.Class
import Data.Generics
import Data.Char
import Data.List
import Data.Either (either)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec (GenParser)
import Text.Printf
import Codec.Binary.UTF8.String

import Unicode
import Tree

type Name = String

data DateTime =
  DateTime {
    year ∷ Int,
    month ∷ Int,
    day ∷ Int,
    hour ∷ Int,
    minute ∷ Int,
    second ∷ Int }
  deriving (Eq,Ord,Data,Typeable)

data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read,Data,Typeable)

data DateInterval = Days ℤ
                  | Weeks ℤ
                  | Months ℤ
                  | Years ℤ
  deriving (Eq,Show,Data,Typeable)

months ∷ [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

showMonth i = capitalize $ months !! (i-1)

instance Show DateTime where
  show (DateTime y m d h min s) = 
    show d ⧺ " " ⧺ showMonth m ⧺ " " ⧺ show y ⧺ ", " ⧺
      show h ⧺ ":" ⧺ show min ⧺ ":" ⧺ show s

data Time = 
  Time {
    tHour ∷ Int,
    tMinute ∷ Int,
    tSecond ∷ Int }
  deriving (Eq,Ord,Show,Data,Typeable)

type Currency = String

type Rates = M.Map (Currency,Currency) Double

showRates rates = unlines $ map showPair $ M.assocs rates
  where
    showPair ((c1,c2), x) = c1 ++ " → " ++ c2 ++ ":\t" ++ show x

data Amount = Double :# Currency
  deriving (Eq,Data,Typeable)
infixr 7 :#

instance Show Amount where
  show (x :# c) = printf "%.2f" x ++ c

data Link a = NoLink
            | LinkTo a
            | ByName Name
  deriving (Eq,Show,Data,Typeable)

data Account = Account {
                accName :: Name,
                accCurrency :: Currency,
                incFrom :: Link Account,
                decTo :: Link Account,
                history :: [(DateTime,Double)] }
  deriving (Eq,Data,Typeable)

instance Show Account where
  show (Account name curr from to hist) = name ++ showFrom from ++ showTo to ++ ":" ++ curr ++ "\n" ++ strHist
    where
      strHist = unlines $ map (\(dt,x) -> show dt ++ ":\t" ++ show x) hist
      
      showFrom NoLink = ""
      showFrom (LinkTo acc) = " <- " ++ accName acc
      showFrom (ByName n) = " <- [" ++ n ++ "]"

      showTo NoLink = ""
      showTo (LinkTo acc) = " -> " ++ accName acc
      showTo (ByName n) = " -> [" ++ n ++ "]"

type AccountsTree = Tree Currency Account

instance Show AccountsTree where
  show (Node name c lst) = name ++ ":" ++ c ++ "\n" ++ show lst
  show (Leaf name acc) = name ++ ":\n" ++ show acc

mkAccMap :: [Account] -> M.Map String Account
mkAccMap lst = M.fromList [(accName a, a) | a <- lst]

data ParserState = 
  ParserState {
    defaultCurrencies :: [Currency]
  }

emptyPState = ParserState []

type MParser a = GenParser Char ParserState a

data LedgerState = LS {
                     now :: DateTime,
                     accounts :: AccountsTree,
                     currentRecord :: Dated Record,
                     records :: [Dated Record],
                     rates :: Rates,
                     templates :: M.Map String Template,
                     ruled :: [(RuleWhen, Rule, Transaction)],
                     messages :: [String] }

instance Show LedgerState where
  show st@(LS now accs _ recs rates _ _ msgs) = unlines $ [show now, showAccs, showRates rates]
                                              ++ ["Balances:\n" ++ showPairs (balances st)]
                                              ++ ["Log:\n" ++ unlines msgs]
--                                               ++ map show recs
    where
      showAccs = unlines $ map show $ leafs accs

setCurrentRecord :: Dated Record -> LState ()
setCurrentRecord rec = do
  st <- get
  put $ st {currentRecord = rec}

data LError = LError {
                eRecord :: Dated Record,
                eState :: LedgerState,
                eReason :: String }

instance Show LError where
  show (LError rec st reason) = encodeString ("Error: " ++ reason ++ " \nAt record:\n" ++ show rec)

newtype AState s a = AState { runState :: s -> Either LError (a, s) }
type LState a = AState LedgerState a

instance Monad (AState LedgerState) where
  return a = AState $ \s -> Right (a,s)
  m >>= k  = AState $ \s -> 
               case runState m s of
                 Right (a,s') -> runState (k a) s'
                 Left err     -> Left err
  fail str = AState $ \s -> Left (LError (currentRecord s) s str)

instance MonadState LedgerState (AState LedgerState) where
    get   = AState $ \s -> Right (s, s)
    put s = AState $ \_ -> Right ((), s)

data Dated a = At {
                 getDate :: DateTime,
                 content :: a }
  deriving (Data,Typeable)

instance (Show a) => Show (Dated a) where
  show (At dt a) = "@" ++ show dt ++ "\n" ++ show a

data Transaction = Transaction {
                status :: Char,
                description :: String,
                parts :: [Posting] }
  deriving (Eq,Data,Typeable)

instance Show Transaction where
  show (Transaction s d ps) = unlines $ (s:' ':d):map show ps

data AmountParam = F Amount
                 | P Double Int Amount
  deriving (Eq, Data,Typeable)

instance Show AmountParam where
  show (F amount) = "F: " ++ show amount
  show (P p n def) = show p ++ "%(#" ++ show n ++ "=" ++ show def ++ ")"

defAmount :: AmountParam -> Amount
defAmount (F amount) = amount
defAmount (P _ _ def) = def

data Posting = Name :<+ AmountParam
             | Auto Name
  deriving (Eq, Data,Typeable)
infixl 6 :<+

instance Show Posting where
  show (s :<+ am) = s ++ " :<+ " ++ show am
  show (Auto acc) = acc ++ " :<+ ???"

data RegularTransaction = RegularTransaction DateTime DateInterval Transaction
  deriving (Show,Data,Typeable)

data Template = Template {
                  tName :: Name,
                  tNParams :: Int,
                  tBody :: Transaction }
  deriving (Data,Typeable)

instance Show Template where
  show (Template name n post) = name ++ "/" ++ show n ++ ":\n" ++ show post

data Record = PR Transaction
            | RR SetRate
            | VR Name Amount
            | RegR RegularTransaction 
            | TR Template
            | CTR Name [Amount]
            | RuledP RuleWhen Rule Transaction
            | RuledC RuleWhen Rule Name [Amount]
  deriving (Data,Typeable)

instance Show Record where
  show (PR post) = show post
  show (RR rr) = show rr
  show (VR acc amount) = acc ++ " == " ++ show amount
  show (RegR rp) = show rp
  show (TR tp) = show tp
  show (CTR name args) = name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
  show (RuledP when rule post) = show when ++ " " ++ show rule ++ " -->\n" ++ show post
  show (RuledC when rule name args) = show when ++ " " ++ show rule ++ " --> " ++ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

data SetRate = Currency := Amount
  deriving (Data,Typeable)
infixl 6 :=

instance Show SetRate where
  show (c := am) = c ++ " := " ++ show am

data Rule = DescrMatch String
          | String :> Amount
          | String :< Amount
  deriving (Show,Data,Typeable)

data RuleWhen = Before | After
  deriving (Show,Read,Data,Typeable)


data Query =
  Q {
    startDate :: Maybe DateTime,
    endDate   :: Maybe DateTime,
    statusIs  :: Maybe Char }
  deriving (Show)

data Condition =
  forall a. Show a => Condition {
    matchFn :: a -> Dated Record -> Bool,
    matchParam :: a,
    areRecordsSortedBy :: Bool }

instance Show Condition where
  show (Condition _ p b) = "<Condition: " ++ show p ++ " " ++ show b ++ ">"

mkDateCondition :: DateTime -> (DateTime -> DateTime -> Bool) -> Condition
mkDateCondition dt op = Condition (\x r -> getDate r `op` x) dt True

data CmdLine =
  CmdLine {
    qFlags :: [QFlag],
    srcFile :: FilePath,
    mode :: Mode }
  deriving (Show)

data QFlag = StartDate DateTime
           | EndDate   DateTime
           | Status    Char
  deriving (Show)

data Mode = Balance
          | Register String
          | Saldo String String String -- Account, start date, end date
  deriving (Show)

data Option = QF QFlag
            | SourceFile FilePath
            | MF Mode
  deriving (Show)

showPairs :: (Show b) => [(String,b)] -> String
showPairs pairs = unlines $ map showPair pairs
  where
    showPair (a,b) = a ++ "\t" ++ show b

balances :: LedgerState -> [(String, Amount)]
balances st = [(accName acc, sumAccount acc) | acc <- leafs $ accounts st]

sumAccount :: Account -> Amount
sumAccount acc = (sum $ map snd $ history acc) :# (accCurrency acc)

readE :: (Read a) => String -> String -> a
readE d s | [x] <- parse = x
          | otherwise    = error $ "readE: Cannot read " ++ d ++ ": «" ++ s ++ "»"
  where
    parse = [x | (x,_) <- reads s]

readM :: (Monad m, Read a) => String -> String -> m a
readM d s | [x] <- parse = return x
          | otherwise    = fail $ "readM: Cannot read " ++ d ++ ": «" ++ s ++ "»"
  where
    parse = [x | (x,_) <- reads s]
                               
