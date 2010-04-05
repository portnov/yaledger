{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, PatternGuards #-}
module Dates
  where

import Data.Char (toUpper)
import Data.Function (on)
import Data.Generics
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec

import Unicode
import Types

showDate :: DateTime -> String
showDate dt = show (year dt) ++ "/" ++ show (month dt) ++ "/" ++ show (day dt)

getCurrentDateTime :: IO DateTime
getCurrentDateTime = do
  zt ← getZonedTime
  let lt = zonedTimeToLocalTime zt
      ld = localDay lt
      ltod = localTimeOfDay lt
      (y,m,d) = toGregorian ld
      h = todHour ltod
      min = todMin ltod
      s = round $ todSec ltod
  return $ DateTime (fromIntegral y) m d h min s

getCurrentDateTime' :: MParser DateTime
getCurrentDateTime' = do
  st <- getState
  return $ currentDateTime st

uppercase ∷ String → String
uppercase = map toUpper

p `isPrefixOfI` s = (uppercase p) `isPrefixOf` (uppercase s)

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k `isPrefixOfI` k' = Just v
                         | otherwise          = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date y m d = DateTime y m d 0 0 0

addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

times ∷ Int → MParser t → MParser [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts

number ∷ Int → Int → MParser Int
number n m = do
  t ← readM "number" =<< (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ MParser Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ MParser Int
pMonth = number 2 12

pDay ∷ MParser Int
pDay = number 2 31

euroNumDate ∷ MParser DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ MParser DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  d ← pDay
  return $ date y m d

euroNumDate' ∷ Int → MParser DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Int → MParser DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  d ← pDay
  return $ date year m d

strDate ∷ MParser DateTime
strDate = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → do
      space
      y ← pYear
      notFollowedBy $ char ':'
      return $ date y m d

strDate' ∷ Int → MParser DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ MParser Time
time24 = do
  h ← number 2 23
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  case x of
    Nothing → return $ Time h m 0
    Just _ → do
      s ← number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm ∷ MParser Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ MParser Time
time12 = do
  h ← number 2 12
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  s ← case x of
            Nothing → return 0
            Just s' → number 2 59
  optional space
  hd ← ampm
  return $ Time (h+hd) m s

pAbsDate ∷ MParser DateTime
pAbsDate = do
  now <- getCurrentDateTime'
  let y = year now
  date ← choice $ map try $ map ($ y) $ [
                              const euroNumDate,
                              const americanDate,
                              const strDate,
                              strDate',
                              euroNumDate',
                              americanDate']
  optional $ char ','
  s ← optionMaybe space
  case s of
    Nothing → return date
    Just _ → do
      t ← choice $ map try [time12,time24]
      return $ date `addTime` t

convertTo dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)
convertFrom dt = 
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

modifyDate fn x dt = convertFrom $ fn x $ convertTo dt

addInterval :: DateTime -> DateInterval -> DateTime
addInterval dt (Days ds) = modifyDate addDays ds dt
addInterval dt (Weeks ws) = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys) = modifyDate addGregorianYearsClip ys dt

datesFromEvery :: DateTime -> DateInterval -> [DateTime]
datesFromEvery dt int = scanl addInterval dt (repeat int)

maybePlural ∷ String → MParser String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateInterval ∷ MParser DateIntervalType
pDateInterval = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  return $ readE "date interval type" s

pRelDate ∷ MParser DateTime
pRelDate = do
  date <- getCurrentDateTime'
  offs ← (try futureDate) <|> (try passDate) <|> (try today) <|> (try tomorrow) <|> yesterday
  return $ date `addInterval` offs

futureDate ∷ MParser DateInterval
futureDate = do
  string "in "
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  case tp of
    Day →   return $ Days (readE "days" n)
    Week →  return $ Weeks (readE "weeks" n)
    Month → return $ Months (readE "months" n)
    Year →  return $ Years (readE "years" n)

passDate ∷ MParser DateInterval
passDate = do
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  string " ago"
  case tp of
    Day →   return $ Days $ - (readE "days" n)
    Week →  return $ Weeks $ - (readE "weeks" n)
    Month → return $ Months $ - (readE "months" n)
    Year →  return $ Years $ - (readE "years" n)

today ∷ MParser DateInterval
today = do
  string "today"
  return $ Days 0

tomorrow ∷ MParser DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday ∷ MParser DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pDate ∷ MParser DateTime
pDate = do
  date <- getCurrentDateTime'
  (try pRelDate) <|> (try pAbsDate)

parseAbsDate :: DateTime -> String -> DateTime
parseAbsDate dt str =
  case runParser pAbsDate (emptyPState dt) "<date>" str of
    Right date -> date
    Left e -> error $ show e

pDateOnly :: MParser DateTime
pDateOnly = do
  date <- getCurrentDateTime'
  let y = year date
  choice $ map try $ map ($ y) $ [
                              const euroNumDate,
                              const americanDate,
                              const strDate,
                              strDate',
                              euroNumDate',
                              americanDate']

pDateOrSeries :: MParser (Either DateTime (DateTime, DateInterval))
pDateOrSeries = do
  date <- getCurrentDateTime'
  (Right `fmap` (try pSeries)) <|> (Left `fmap` (try pAbsDate))

pSeries :: MParser (DateTime, DateInterval)
pSeries = do
  dt <- pDateOnly
  spaces
  string "every"
  spaces
  n <- (readE "periods number") `fmap` (many1 digit)
  char ' '
  tp <- pDateInterval 
  let int = case tp of
             Day -> Days n
             Week -> Weeks n
             Month -> Months n
             Year -> Years n
  return (dt, int)

