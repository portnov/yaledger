{-# LANGUAGE MultiParamTypeClasses #-}
module YaLedger.Output.Tables where

data Align = ALeft | ACenter | ARight
  deriving (Eq, Show)

type Column = [String]
type Row = [Column]

class TableFormat a where
  tableColumns :: a -> [([String], Align, Column)] -> Column

  tableGrid :: a -> [(Align, [String])] -> [Row] -> Column

  maxFieldWidth :: a -> Maybe Int
  maxFieldWidth _ = Nothing

  showFooter :: a -> String -> Column
  showFooter _ s = [s]

data TableColumn a = TableColumn {
    columnTitle :: Column,
    columnAlign :: Align,
    columnGetter :: a -> Column }

class ToTable opts s where
  shownColumns :: opts -> [TableColumn s]

padColumns :: Row -> Row
padColumns columns =
  let m = maximum (map length columns)
      padE n x
        | length x >= n = x
        | otherwise = x ++ replicate (n - length x) ""
  in  map (padE m) columns

