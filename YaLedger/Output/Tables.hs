{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module YaLedger.Output.Tables where

import YaLedger.Output.ANSI

data Align = ALeft | ACenter | ARight
  deriving (Eq, Show)

type Column = [TextOutput]
type Row = [Column]

class TableFormat a where
  tableColumns :: a -> [([TextOutput], Align, Column)] -> Column

  tableGrid :: a -> [(Align, [TextOutput])] -> [Row] -> Column

  maxFieldWidth :: a -> Maybe Int
  maxFieldWidth _ = Nothing

  showFooter :: a -> TextOutput -> Column
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
        | otherwise = x ++ replicate (n - length x) emptyText
  in  map (padE m) columns

