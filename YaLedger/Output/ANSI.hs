{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module YaLedger.Output.ANSI
  (Color (..),
   OutAttributes (..), Fragment (..), FormattedText,
   output, boldText,
   (<>),
   bold, color, dull, faint,
   toString, textLength, takeText,
   unlinesText, unwordsText,
   emptyText, space, spaces, newline, vbar,
   putText, putTextLn
  ) where

import Data.String
import Data.List
import System.IO
import System.Console.ANSI

data OutAttributes =
       OutAttributes {
        aColor :: Maybe Color,
        aColorIntensity :: Maybe ColorIntensity,
        aConsoleIntensity :: Maybe ConsoleIntensity}
    | Plain
    deriving (Eq, Show)

toSGR :: OutAttributes -> [SGR]
toSGR Plain = [Reset]
toSGR (OutAttributes Nothing Nothing Nothing) = [Reset]
toSGR (OutAttributes (Just clr) (Just i) Nothing) = [SetColor Foreground i clr]
toSGR (OutAttributes (Just clr) (Just i) (Just c)) = [SetColor Foreground i clr, SetConsoleIntensity c]
toSGR (OutAttributes (Just clr) Nothing Nothing) = [SetColor Foreground Dull clr]
toSGR (OutAttributes (Just clr) Nothing (Just c)) = [SetColor Foreground Dull clr, SetConsoleIntensity c]
toSGR (OutAttributes Nothing (Just i) Nothing) = [SetColor Foreground i Black]
toSGR (OutAttributes Nothing (Just i) (Just c)) = [SetColor Foreground i Black, SetConsoleIntensity c]
toSGR (OutAttributes Nothing Nothing (Just c)) = [SetConsoleIntensity c]

data Fragment = Fragment {
      fAttributes :: OutAttributes,
      fText :: String }
  deriving (Eq)

instance Show Fragment where
  show f = fText f

type FormattedText = [Fragment]

bold :: OutAttributes
bold = OutAttributes Nothing Nothing (Just BoldIntensity)

color :: Color -> OutAttributes
color clr = OutAttributes (Just clr) Nothing Nothing

dull :: OutAttributes
dull = OutAttributes Nothing (Just Dull) Nothing

faint :: OutAttributes
faint = OutAttributes Nothing Nothing (Just FaintIntensity)

instance IsString FormattedText where
  fromString str = [Fragment Plain str] 

class ToOutput a where
  toOutput :: a -> FormattedText

instance ToOutput String where
  toOutput str = [Fragment Plain str]

instance ToOutput FormattedText where
  toOutput = id

(<>) :: (ToOutput a, ToOutput b) => a -> b -> FormattedText
x <> y = toOutput x ++ toOutput y

output :: String -> FormattedText
output = toOutput

boldText :: String -> FormattedText
boldText str = [Fragment bold str]

toString :: FormattedText -> String
toString t = concatMap fText t

textLength :: FormattedText -> Int
textLength t = sum $ map (length . fText) t

takeText :: Int -> FormattedText -> FormattedText
takeText _ [] = []
takeText n (f:fs)
  | length (fText f) == n = [f]
  | length (fText f) > n = [f {fText = take n (fText f)}]
  | otherwise = f: takeText (n - length (fText f)) fs

emptyText :: FormattedText
emptyText = []

space :: FormattedText
space = " "

newline :: FormattedText
newline = "\n"

spaces :: Int -> FormattedText
spaces n = fromString $ replicate n ' '

vbar :: FormattedText
vbar = [Fragment bold "│"]

unlinesText :: [FormattedText] -> FormattedText
unlinesText texts = intercalate newline texts

unwordsText :: [FormattedText] -> FormattedText
unwordsText texts = intercalate space texts

putText :: Bool -> FormattedText -> IO ()
putText colorize fs = do
    tty <- hIsTerminalDevice stdout
    if tty && colorize
      then mapM_ put fs
      else putStr $ toString fs
  where
    put (Fragment Plain str) = putStr str
    put (Fragment attr str) = do
        setSGR (toSGR attr)
        putStr str
        setSGR [Reset]

putTextLn :: Bool -> FormattedText -> IO ()
putTextLn colorize text = putText colorize text >> putStrLn ""

