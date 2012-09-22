
module YaLedger.Strings where

data Align = ALeft | ACenter | ARight
  deriving (Eq, Show)

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

alignMax :: Align -> [String] -> [String]
alignMax a list =
  let m = maximum (map length list)
  in  map (pad . align m a) list

pad :: String -> String
pad s = " " ++ s ++ " "

zipS :: String -> [String] -> [String] -> [String]
zipS sep l1 l2 =
  let m = max (length l1) (length l2)
      l1' = take m $ map Just l1 ++ repeat Nothing
      l2' = take m $ map Just l2 ++ repeat Nothing
      s Nothing = replicate m ' '
      s (Just x) = x
      go x y = s x ++ sep ++ s y
  in  zipWith go l1' l2'

twoColumns :: String -> String -> [String] -> [String] -> [String]
twoColumns h1 h2 l1 l2 =
  let m1 = maximum (map length (h1:l1))
      m2 = maximum (map length (h2:l2))
      s1 = replicate m1 '='
      s2 = replicate m2 '='
      h1' = align m1 ACenter h1
      h2' = align m2 ACenter h2
  in  zipS "|" (h1':s1:l1) (h2':s2:l2)

columns :: [(String, [String])] -> [String]
columns list =
  let ms = [maximum (map length (h:l)) | (h, l) <- list]
      ss = [replicate m '=' | m <- ms]
      hs = map fst list
      bs = map snd list
  in  foldr1 (zipS "|") [h:s:l | (h,s,l) <- zip3 hs ss bs]

columns' :: [[String]] -> [String]
columns' list = foldr1 (zipS "|") list

understrike :: [String] -> [String]
understrike list =
  let m = maximum (map length list)
  in  list ++ [replicate m '=']

