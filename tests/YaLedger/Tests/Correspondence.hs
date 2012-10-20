{-# LANGUAGE ScopedTypeVariables #-}
module YaLedger.Tests.Correspondence where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.List

import YaLedger.Types
import YaLedger.Monad
import YaLedger.Kernel
import YaLedger.Logger
import YaLedger.Exceptions
import YaLedger.Tests.Instances

loadAccs :: FilePath -> IO [[(String, String)]]
loadAccs filepath = do
  content <- readFile filepath
  let ls = lines content
      grps = split "" ls
  forM grps $ \list ->
    forM list $ \line ->
      let [x, y] = words line
      in  return (x, y)

testCheckEntry date attrs uentry creditAccPaths debitAccPaths = do
  centry <- checkEntry date attrs uentry
  let debitAccs  = map postingAccount $ cEntryDebitPostings centry
      creditAccs = map postingAccount $ cEntryCreditPostings centry
  coa <- gets lsCoA
  forM_ (zip creditAccPaths creditAccs) $ \(path, acc) -> do
      let Just accPath = accountFullPath (getID acc) coa
          reqPath = mkPath path
      when (reqPath /= accPath) $
          failTest "correspondence" $ path ++ " /= " ++ intercalate "/" accPath

correspondenceTest currs coa amap options records = do
  accs <- loadAccs "correspondence.txt"
  let entries = [p | p@(Ext {getContent = Transaction (TEntry _)}) <- records]
  runLedger options coa amap records $ runEMT $ do
    forM_ (zip entries accs) $ \(rec, list) -> do
      let cra = map fst list
          dta = map snd list
      let Transaction (TEntry entry) = getContent rec
      setPos (getLocation rec)
      testCheckEntry (getDate rec) (getAttributes rec) entry cra dta
        `catchWithSrcLoc`
          (\l (e :: NoCorrespondingAccountFound) -> handler l e)
        `catchWithSrcLoc`
          (\l (e :: NoSuchRate) -> handler l e)
        `catchWithSrcLoc`
          (\l (e :: InvalidAccountType) -> handler l e)
        `catchWithSrcLoc`
          (\l (e :: TestFailed) -> handler l e)
