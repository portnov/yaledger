
module YaLedger.Installer
  (install
  ) where

import System.FilePath
import System.Directory

import qualified Paths_yaledger as Paths

install :: Maybe FilePath -> IO ()
install Nothing = do
  configDir <- getUserConfigDir "yaledger"
  doInstall configDir
install (Just dir) = doInstall dir

doInstall :: FilePath -> IO ()
doInstall configDir = do
  let mainConfigPath  = configDir </> "yaledger.yaml"
      chartOfAccounts = configDir </> "default.accounts"
      currenciesList  = configDir </> "currencies.yaml"

  mainConfigSrc      <- Paths.getDataFileName "yaledger.yaml"
  chartOfAccountsSrc <- Paths.getDataFileName "default.accounts"
  currenciesSrc      <- Paths.getDataFileName "currencies.yaml"

  copyFile mainConfigSrc      mainConfigPath
  copyFile chartOfAccountsSrc chartOfAccounts
  copyFile currenciesSrc      currenciesList 

