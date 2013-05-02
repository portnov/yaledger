{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Output.ASCII where

import Data.List
import Data.String

import YaLedger.Output.Formatted
import YaLedger.Output.Tables

data ASCII = ASCII
  deriving (Eq, Show)

align :: Int -> Align -> FormattedText -> FormattedText
align w ALeft str
  | textLength str >= w = takeText w str
  | otherwise = str <> replicate (w - textLength str) ' '
align w ARight str
  | textLength str >= w = takeText w str
  | otherwise = replicate (w - textLength str) ' ' <> str
align w ACenter str
  | textLength str >= w = takeText w str
  | otherwise =
    let m = (w - textLength str) `div` 2
        n = w - textLength str - m
        pad1 = replicate m ' '
        pad2 = replicate n ' '
    in pad1 <> str <> pad2

alignPad :: Int -> Align -> FormattedText -> FormattedText
alignPad w ALeft str
  | textLength str >= w = takeText w str
  | otherwise = space <> str <> spaces (w - textLength str - 1)
alignPad w ARight str
  | textLength str >= w = takeText w str
  | otherwise = spaces (w - textLength str - 1)  <> str <> space
alignPad w ACenter str
  | textLength str >= w = takeText w str
  | otherwise =
    let m = (w - textLength str) `div` 2
        n = w - textLength str - m
        pad1 = spaces m
        pad2 = spaces n
    in pad1 <> str <> pad2

alignMax :: Align -> Column -> Column 
alignMax a list =
  let m = maximum (map textLength list)
  in  map (pad . align m a) list

pad :: FormattedText -> FormattedText
pad s = space <> s <> space

zipS :: FormattedText -> Column -> Column -> Column
zipS sep l1 l2 =
  let m = max (length l1) (length l2)
      m1 = if null l1 then 0 else maximum (map textLength l1)
      m2 = if null l2 then 0 else maximum (map textLength l2)
      l1' = take m $ map Just l1 ++ repeat Nothing
      l2' = take m $ map Just l2 ++ repeat Nothing
      s n Nothing = spaces n
      s _ (Just x) = x
      go x y = s m1 x <> sep <> s m2 y
  in  zipWith go l1' l2'

twoColumns :: FormattedText -> FormattedText -> Column -> Column -> Column
twoColumns h1 h2 l1 l2 =
  let m1 = maximum (map textLength (h1:l1))
      m2 = maximum (map textLength (h2:l2))
      h1' = align m1 ACenter h1
      h2' = align m2 ACenter h2
  in  tabline TopLine [m1,m2]:
      (vbar <> h1' <> vbar <> h2' <> vbar):
      tabline MidLine [m1,m2]:
      map (\l -> vbar <> l <> vbar) (zipS vbar l1 l2) ++
      [tabline BottomLine [m1, m2]]

columns' :: [Column] -> Column
columns' list = foldr (zipS vbar) [] list

understrike :: Column -> Column
understrike list =
  let m = maximum (map textLength list)
  in  list ++ [fromString $ replicate m '═']

data LineKind = TopLine | MidLine | BottomLine
  deriving (Eq, Show)

startchar :: LineKind -> Char
startchar TopLine    = '╒'
startchar MidLine    = '╞'
startchar BottomLine = '╘'

midchar :: LineKind -> Char
midchar TopLine    = '╤'
midchar MidLine    = '╪'
midchar BottomLine = '╧'

endchar :: LineKind -> Char
endchar TopLine    = '╕'
endchar MidLine    = '╡'
endchar BottomLine = '╛'

tabline :: LineKind -> [Int] -> FormattedText
tabline k ms = boldText $ startchar k: concatMap go (init ms) ++ line (last ms) ++ [endchar k]
  where
    go m = line m ++ [midchar k]
    line m = replicate m '═'

instance TableFormat ASCII where
  tableColumns ASCII list =
    let ms = [(a, maximum (map textLength (h ++ l)) + 2) | (h, a, l) <- list]
        ws = map snd ms
        ss = [replicate m '═' | (_,m) <- ms]
        hs = map (\(x,_,_) -> x) list
        bs = map (\(_,_,x) -> x) list
    in  tabline TopLine ws :
        map (vbar <>) ( foldr (zipS vbar) [] [map (alignPad m ACenter) h | (h,(a,m),s,l) <- zip4 hs ms ss bs] ) ++
        [tabline MidLine ws] ++
        map (vbar <>) ( foldr (zipS vbar) [] [map (alignPad m a) l | (h,(a,m),s,l) <- zip4 hs ms ss bs] ) ++
        [tabline BottomLine ws]

  tableGrid ASCII _ [] = []
  tableGrid ASCII colHeaders rows =
    let headers = map snd colHeaders
        aligns  = map fst colHeaders
        rows' = map padColumns rows :: [Row]
        cols = foldr1 (zipWith (++)) rows' :: Row
        wds = [maximum $ map textLength (h ++ column) | (h,column) <- zip headers cols]
        colsAligned = [map (align (w+2) a) col | (w,col,a) <- zip3 wds cols aligns]
        headersAligned = [map (align (w+2) ACenter) h | (w,h) <- zip wds headers]
    in  tableColumns ASCII $ zip3 headersAligned aligns colsAligned

  maxFieldWidth ASCII = Just 24

