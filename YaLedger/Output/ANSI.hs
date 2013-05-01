{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module YaLedger.Output.ANSI
  (Color (..),
   OutAttributes (..), Fragment (..), TextOutput,
   output, boldText,
   (<>),
   bold, color,
   toString, textLength, takeText,
   unlinesText, unwordsText,
   emptyText, space, spaces, newline, vbar,
   putText, putTextLn
  ) where

import Data.String
import Data.List
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

type TextOutput = [Fragment]

bold :: OutAttributes
bold = OutAttributes Nothing Nothing (Just BoldIntensity)

color :: Color -> OutAttributes
color clr = OutAttributes (Just clr) Nothing Nothing

instance IsString TextOutput where
  fromString str = [Fragment Plain str] 

class ToOutput a where
  toOutput :: a -> TextOutput

instance ToOutput String where
  toOutput str = [Fragment Plain str]

instance ToOutput TextOutput where
  toOutput = id

(<>) :: (ToOutput a, ToOutput b) => a -> b -> TextOutput
x <> y = toOutput x ++ toOutput y

output :: String -> TextOutput
output = toOutput

boldText :: String -> TextOutput
boldText str = [Fragment bold str]

toString :: TextOutput -> String
toString t = concatMap fText t

textLength :: TextOutput -> Int
textLength t = sum $ map (length . fText) t

takeText :: Int -> TextOutput -> TextOutput
takeText _ [] = []
takeText n (f:fs)
  | length (fText f) == n = [f]
  | length (fText f) < n = [f {fText = take n (fText f)}]
  | otherwise = f: takeText (n - length (fText f)) fs

emptyText :: TextOutput
emptyText = []

space :: TextOutput
space = " "

newline :: TextOutput
newline = "\n"

spaces :: Int -> TextOutput
spaces n = fromString $ replicate n ' '

vbar :: TextOutput
vbar = [Fragment bold "│"]

unlinesText :: [TextOutput] -> TextOutput
unlinesText texts = intercalate newline texts

unwordsText :: [TextOutput] -> TextOutput
unwordsText texts = intercalate space texts

putText :: TextOutput -> IO ()
putText fs = mapM_ put fs
  where
    put (Fragment Plain str) = putStr str
    put (Fragment attr str) = do
        setSGR (toSGR attr)
        putStr str
        setSGR [Reset]

putTextLn :: TextOutput -> IO ()
putTextLn text = putText text >> putStrLn ""

