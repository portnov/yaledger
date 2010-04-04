
import System.Environment

import Types
import Dates
import TopLevel
import CmdLine
import Queries

main = do
  now <- getCurrentDateTime
  args <- getArgs
  cmd <- parseCmdLine now args
  let qry =  parseQFlags now (qFlags cmd)
      pred = buildCondition qry
  (accs, recs) <- readLedger now (srcFile cmd)
  let st = runQuery now pred accs recs
  case mode cmd of
    Balance -> printBalance st
    Register path -> printRegister st path
    Saldo path start end -> print $ saldo now pred accs recs path start end


