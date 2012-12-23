
module YaLedger.Installer
  (install
  ) where

import System.FilePath
import System.Directory
import System.Environment.XDG.BaseDir

import qualified Paths_yaledger as Paths

synopsis :: String
synopsis = "Usage: yaledger init [DIRECTORY]"

install :: [String] -> IO ()
install [] = do
  configDir <- getUserConfigDir "yaledger"
  doInstall configDir
install ["-h"]     = putStrLn synopsis
install ["--help"] = putStrLn synopsis
install [dir] = doInstall dir
install _ = 
  putStrLn $ "Too many command line arguments.\n" ++ synopsis

doInstall :: FilePath -> IO ()
doInstall configDir = do
  putStrLn $ "Installing sample YaLedger configs into " ++ configDir
  createDirectoryIfMissing True configDir

  let mainConfigPath  = configDir </> "yaledger.yaml"
      chartOfAccounts = configDir </> "default.accounts"
      currenciesList  = configDir </> "currencies.yaml"

  mainConfigSrc      <- Paths.getDataFileName "configs/yaledger.yaml"
  chartOfAccountsSrc <- Paths.getDataFileName "configs/default.accounts"
  currenciesSrc      <- Paths.getDataFileName "configs/currencies.yaml"

  copyFile mainConfigSrc      mainConfigPath
  copyFile chartOfAccountsSrc chartOfAccounts
  copyFile currenciesSrc      currenciesList 

  putStrLn "done."

