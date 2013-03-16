
module YaLedger.Output.ASCII where

import Data.List

import YaLedger.Output.Tables

data ASCII = ASCII

align :: Int -> Align -> String -> String
align w ALeft str
  | length str >= w = take w str
  | otherwise = str ++ replicate (w - length str) ' '
align w ARight str
  | length str >= w = take w str
  | otherwise = replicate (w - length str) ' ' ++ str
align w ACenter str
  | length str >= w = take w str
  | otherwise =
    let m = (w - length str) `div` 2
        n = w - length str - m
        pad1 = replicate m ' '
        pad2 = replicate n ' '
    in pad1 ++ str ++ pad2

alignPad :: Int -> Align -> String -> String
alignPad w ALeft str
  | length str >= w = take w str
  | otherwise = " " ++ str ++ replicate (w - length str - 1) ' '
alignPad w ARight str
  | length str >= w = take w str
  | otherwise = replicate (w - length str - 1) ' ' ++ str ++ " " 
alignPad w ACenter str
  | length str >= w = take w str
  | otherwise =
    let m = (w - length str) `div` 2
        n = w - length str - m
        pad1 = replicate m ' '
        pad2 = replicate n ' '
    in pad1 ++ str ++ pad2

alignMax :: Align -> Column -> Column 
alignMax a list =
  let m = maximum (map length list)
  in  map (pad . align m a) list

pad :: String -> String
pad s = " " ++ s ++ " "

zipS :: String -> Column -> Column -> Column
zipS sep l1 l2 =
  let m = max (length l1) (length l2)
      l1' = take m $ map Just l1 ++ repeat Nothing
      l2' = take m $ map Just l2 ++ repeat Nothing
      s Nothing = replicate m ' '
      s (Just x) = x
      go x y = s x ++ sep ++ s y
  in  zipWith go l1' l2'

twoColumns :: String -> String -> Column -> Column -> Column
twoColumns h1 h2 l1 l2 =
  let m1 = maximum (map length (h1:l1))
      m2 = maximum (map length (h2:l2))
      h1' = align m1 ACenter h1
      h2' = align m2 ACenter h2
  in  tabline TopLine [m1,m2]:
      ("│" ++ h1' ++ "│" ++ h2' ++ "│"):
      tabline MidLine [m1,m2]:
      map (\l -> '│':l ++ "│") (zipS "│" l1 l2) ++
      [tabline BottomLine [m1, m2]]

columns' :: [Column] -> Column
columns' list = foldr (zipS "│") [] list

understrike :: Column -> Column
understrike list =
  let m = maximum (map length list)
  in  list ++ [replicate m '═']

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

tabline :: LineKind -> [Int] -> String
tabline k ms = startchar k: concatMap go (init ms) ++ line (last ms) ++ [endchar k]
  where
    go m = line m ++ [midchar k]
    line m = replicate m '═'

instance TableFormat ASCII where
  tableColumns ASCII list =
    let ms = [(a, maximum (map length (h ++ l)) + 2) | (h, a, l) <- list]
        ws = map snd ms
        ss = [replicate m '═' | (_,m) <- ms]
        hs = map (\(x,_,_) -> x) list
        bs = map (\(_,_,x) -> x) list
    in  tabline TopLine ws :
        map ('│':) ( foldr (zipS "│") [] [map (alignPad m ACenter) h | (h,(a,m),s,l) <- zip4 hs ms ss bs] ) ++
        [tabline MidLine ws] ++
        map ('│':) ( foldr (zipS "│") [] [map (alignPad m a) l | (h,(a,m),s,l) <- zip4 hs ms ss bs] ) ++
        [tabline BottomLine ws]

  tableGrid ASCII _ [] = []
  tableGrid ASCII colHeaders rows =
    let headers = map snd colHeaders
        aligns  = map fst colHeaders
        rows' = map padColumns rows :: [Row]
        cols = foldr1 (zipWith (++)) rows' :: Row
        wds = [maximum $ map length (h ++ column) | (h,column) <- zip headers cols]
        colsAligned = [map (align (w+2) a) col | (w,col,a) <- zip3 wds cols aligns]
        headersAligned = [map (align (w+2) ACenter) h | (w,h) <- zip wds headers]
    in  tableColumns ASCII $ zip3 headersAligned aligns colsAligned

  maxFieldWidth ASCII = Just 24

