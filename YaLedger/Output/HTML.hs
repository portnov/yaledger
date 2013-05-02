
module YaLedger.Output.HTML
  (HTML (..)
  ) where

import Data.List
import Text.Printf
import System.Console.ANSI

import YaLedger.Output.Formatted hiding (color)
import YaLedger.Output.Tables

data HTML = HTML
  deriving (Eq, Show)

weight NormalIntensity = ""
weight FaintIntensity = "font-weight: lighter"
weight BoldIntensity  = "font-weight: bold"

color clr Dull = "color: " ++ show clr
color clr Vivid = "color: light" ++ show clr

getStyle Plain = ""
getStyle (OutAttributes Nothing Nothing Nothing) = ""
getStyle (OutAttributes Nothing Nothing (Just c)) = weight c
getStyle (OutAttributes Nothing (Just i) Nothing) = color Black i
getStyle (OutAttributes Nothing (Just i) (Just c)) = color Black i ++ "; " ++ weight c
getStyle (OutAttributes (Just clr) Nothing Nothing) = color clr Dull
getStyle (OutAttributes (Just clr) Nothing (Just c)) = color clr Dull ++ "; " ++ weight c
getStyle (OutAttributes (Just clr) (Just i) Nothing) = color clr i 
getStyle (OutAttributes (Just clr) (Just i) (Just c)) = color clr i ++ "; " ++ weight c

alignedCell :: Align -> [FormattedText] -> String
alignedCell align cell = printf "<td align='%s'>%s</td>" (cssAlign align) (unlinesHTML cell)

cssAlign :: Align -> String
cssAlign ARight  = "right"
cssAlign ACenter = "center"
cssAlign ALeft   = "left"

formatHTML :: FormattedText -> String
formatHTML fs = concatMap go fs
  where
    go (Fragment Plain text) = text
    go (Fragment attrs text) = printf "<span style='%s'>%s</span>" (getStyle attrs) text

toRows :: [(hdr, Align, [cell])] -> [[(Align, cell)]]
toRows list = 
  let aligns = map (\(_,a,_) -> a) list
      texts = transpose $ map (\(_,_,x) -> x) list
      go a t = (a,t)
  in  map (zipWith go aligns) texts

mkHeader :: [FormattedText] -> String
mkHeader hdrs = "<tr>" ++ concatMap th hdrs ++ "</tr>"
  where
    th text = printf "<th>%s</th>" (formatHTML text)

unlinesHTML :: [FormattedText] -> String
unlinesHTML ls = intercalate "<br/>" $ map formatHTML ls

formatRow :: [(Align, [FormattedText])] -> String
formatRow cells = "<tr>" ++ concatMap go cells ++ "</tr>"
  where
    go = uncurry alignedCell

formatRow' :: [(Align, FormattedText)] -> String
formatRow' cells = formatRow [(a, [t]) | (a,t) <- cells]

instance TableFormat HTML where
  tableColumns HTML list = map output $
      [ "<table style='white-space: pre'>"
      , mkHeader getHeader ] ++
      map formatRow' (toRows list) ++
      [ "</table>" ]
    where
      getHeader = map (\(h,_,_) -> concat h) list

  tableGrid HTML cols rows = map output $
      [ "<table style='white-space: pre'>"
      , mkHeader getHeader ] ++
      map formatRow alignedRows ++
      [ "</table>" ]
    where
      getHeader = map (\(_,h) -> concat h) cols
      alignedRows = map (zipWith go aligns) rows
      aligns = map fst cols
      go a t = (a,t)

  formatLine HTML str = printf "<p>%s</p>" str

  formatHeader HTML =
    output $
      "<html>\n" ++
      "<header>\n" ++
      "<style>\n" ++
      "  td, th {border-right: 1px black solid; padding-left: 1em; padding-right: 1em;}\n" ++
      "  th {border-bottom: 1px black solid;}\n" ++
      "  table {border: 1px black solid;}\n" ++
      "</style>\n" ++
      "<title>YaLedger Report</title>\n" ++
      "</header>\n" ++
      "<body style='font-family: monospace'>\n"

  formatTrailer HTML = output "</body>\n</html>"

