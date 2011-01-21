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

-- | Currencies rates
type Rates = M.Map (Currency,Currency) Double

showRates rates = unlines $ map showPair $ M.assocs rates
  where
    showPair ((c1,c2), x) = c1 ++ " → " ++ c2 ++ ":\t" ++ show x

-- | Some amount of money in specified currency
data Amount = Double :# Currency
  deriving (Eq,Data,Typeable)
infixr 7 :#

instance Show Amount where
  show (x :# c) = printf "%.2f" x ++ c

-- | A link from one account to another
data Link a = NoLink
            | LinkTo a
            | ByName Name
  deriving (Eq,Show,Data,Typeable)

-- | One account
data Account = Account {
                accName :: Name,            -- ^ Account name
                accCurrency :: Currency,    -- ^ Account currency
                incFrom :: Link Account,    -- ^ Where to get money from when striking balance
                decTo :: Link Account,      -- ^ Where to put money to when striking balance
                hold :: Amount,             -- ^ Holded amount
                history :: [(DateTime,Double)] -- ^ History of account
              }
  deriving (Eq,Data,Typeable)

instance Show Account where
  show (Account name curr from to _ hist) = name ++ showFrom from ++ showTo to ++ ":" ++ curr ++ "\n" ++ strHist
    where
      strHist = unlines $ map (\(dt,x) -> show dt ++ ":\t" ++ show x) hist
      
      showFrom NoLink = ""
      showFrom (LinkTo acc) = " <- " ++ accName acc
      showFrom (ByName n) = " <- [" ++ n ++ "]"

      showTo NoLink = ""
      showTo (LinkTo acc) = " -> " ++ accName acc
      showTo (ByName n) = " -> [" ++ n ++ "]"

-- | Tree of accounts
type AccountsTree = Tree Currency Account

instance Show AccountsTree where
  show (Node name c lst) = name ++ ":" ++ c ++ "\n" ++ show lst
  show (Leaf name acc) = name ++ ":\n" ++ show acc

mkAccMap :: [Account] -> M.Map String Account
mkAccMap lst = M.fromList [(accName a, a) | a <- lst]

-- | State of the parser
data ParserState = 
  ParserState {
    defaultCurrencies :: [Currency], -- ^ Stack of default currencies
    currentDateTime :: DateTime      -- ^ Current date / time
  }

-- | Empty (start) parser state
emptyPState :: DateTime -> ParserState
emptyPState = ParserState []

-- | Used parser monad
type MParser a = GenParser Char ParserState a

-- | State of calculation
data LedgerState = LS {
                     now :: DateTime,                          -- ^ Current date / time
                     accounts :: AccountsTree,                 -- ^ Accounts tree
                     currentRecord :: Dated Record,            -- ^ Currently calculated record
                     records :: [Dated Record],                -- ^ All records
                     rates :: Rates,                           -- ^ Current rates
                     templates :: M.Map String Template,       -- ^ Record templates
                     ruled :: [(RuleWhen, Rule, Transaction)], -- ^ Rules for transactions
                     messages :: [String]                      -- ^ Log messages
                  }

instance Show LedgerState where
  show st@(LS now accs _ recs rates _ _ msgs) = unlines $ [show now, showAccs, showRates rates]
                                              ++ ["Balances:\n" ++ showPairs (balances st)]
                                              ++ ["Log:\n" ++ unlines msgs]
--                                               ++ map show recs
    where
      showAccs = unlines $ map show $ leafs accs

-- | Set record as current
setCurrentRecord :: Dated Record -> LState ()
setCurrentRecord rec = do
  st <- get
  put $ st {currentRecord = rec}

-- | Calculation error
data LError = LError {
                eRecord :: Dated Record,
                eState :: LedgerState,
                eReason :: String }

instance Show LError where
  show (LError rec st reason) = encodeString ("Error: " ++ reason ++ " \nAt record:\n" ++ show rec)

-- | Generic state/error monad type
newtype AState s a = AState { runState :: s -> Either LError (a, s) }

-- | Concrete monad used for calculations
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

-- | A transaction
data Transaction = Transaction {
                status :: Char,        -- ^ User-specified transaction status
                description :: String, -- ^ Transaction description
                parts :: [Posting]     -- ^ Parts of transaction
              }
  deriving (Eq,Data,Typeable)

instance Show Transaction where
  show (Transaction s d ps) = unlines $ (s:' ':d):map show ps

-- | Concrete amount or template parameter
data AmountParam = F Amount
                 | P Double Int Amount -- ^ Parameter number and default value
  deriving (Eq, Data,Typeable)

instance Show AmountParam where
  show (F amount) = "F: " ++ show amount
  show (P p n def) = show p ++ "%(#" ++ show n ++ "=" ++ show def ++ ")"

defAmount :: AmountParam -> Amount
defAmount (F amount) = amount
defAmount (P _ _ def) = def

-- | One posting (sub-entry)
data Posting = Name :<+ AmountParam
             | Auto Name
  deriving (Eq, Data,Typeable)
infixl 6 :<+

instance Show Posting where
  show (s :<+ am) = s ++ " :<+ " ++ show am
  show (Auto acc) = acc ++ " :<+ ???"

-- | Scheduled regular transaction
data RegularTransaction = RegularTransaction DateTime DateInterval Transaction
  deriving (Show,Data,Typeable)

-- | Transaction template
data Template = Template {
                  tName :: Name,         -- ^ Template name
                  tNParams :: Int,       -- ^ Number of template's parameters
                  tBody :: Transaction   -- ^ Templated transaction itself
                }
  deriving (Data,Typeable)

instance Show Template where
  show (Template name n post) = name ++ "/" ++ show n ++ ":\n" ++ show post

-- | Generic ledger record
data Record = PR Transaction
            | RR SetRate
            | VR Name Amount
            | RegR RegularTransaction 
            | TR Template
            | CTR Name [Amount]
            | Hold Name Amount
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
  show (Hold name a) = "Hold " ++  show a ++ " on " ++ name
  show (RuledP when rule post) = show when ++ " " ++ show rule ++ " -->\n" ++ show post
  show (RuledC when rule name args) = show when ++ " " ++ show rule ++ " --> " ++ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

-- | «Set rate» record
data SetRate = Currency := Amount
  deriving (Data,Typeable)
infixl 6 :=

instance Show SetRate where
  show (c := am) = c ++ " := " ++ show am

data Rule = DescrMatch String -- ^ Match description
          | String :> Amount  -- ^ Match when amount put to given account is greater then given amount
          | String :< Amount  -- ^ same, but «less then»
  deriving (Show,Data,Typeable)

-- | Execute automatic transaction before or after transaction which triggered rule
data RuleWhen = Before | After
  deriving (Show,Read,Data,Typeable)

-- | Balance
data ABalance = ABalance {
                 fullSum :: Amount,
                 available :: Amount }

instance Show ABalance where
  show (ABalance s a) = 
    if s == a
      then show s
      else show s ++ " (available " ++ show a ++ ")"

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

data ConditionType = OnStartDate
                   | OnEndDate
                   | OnStatus
  deriving (Eq,Ord,Show)

newtype Conditions = Conditions (M.Map ConditionType Condition)

instance Show Conditions where
  show (Conditions mm) = show (M.elems mm)

addCondition :: Conditions -> (ConditionType, Condition) -> Conditions
addCondition (Conditions mm) (k,v) = Conditions $ M.insert k v mm

buildConditions :: [(ConditionType, Condition)] -> Conditions
buildConditions = Conditions . M.fromList

conditions :: Conditions -> [Condition]
conditions (Conditions mm) = M.elems mm

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

sumAccount' :: DateTime -> DateTime -> Account -> Amount
sumAccount' start end acc = (sum $ map snd $ filter good $ history acc) :# (accCurrency acc)
  where
    good (dt,_) = (dt >= start) && (dt <= end)

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
                               
