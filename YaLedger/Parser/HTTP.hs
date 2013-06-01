
module YaLedger.Parser.HTTP where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import Data.Char (ord)
import Network.HTTP
import Network.Browser

toByteString :: String -> B.ByteString
toByteString str = B.pack $ map (fromIntegral . ord) str

loadHTTP :: String -> IO String
loadHTTP url = do
  (_, rsp) <- browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest url
  return $ rspBody rsp

loadHTTPText :: String -> IO T.Text
loadHTTPText url = do
  rsp <- loadHTTP url
  let bstr = toByteString rsp
  return $ E.decodeUtf8 bstr

