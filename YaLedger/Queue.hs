
module YaLedger.Queue where

import Control.Concurrent.STM
import qualified Data.PQueue.Prio.Min as Q

import YaLedger.Types.Common

newQueue :: IO (Queue a)
newQueue = newTVarIO Q.empty

enqueue :: Ext a -> Queue a -> STM ()
enqueue x queue =
  modifyTVar queue $
    \q -> Q.insert (getDate x) x q
  
getFromQueue :: Queue a -> STM (Maybe (Ext a))
getFromQueue queue = do
  q <- readTVar queue
  case Q.minView q of
    Just (x, q') -> do
                    writeTVar queue q'
                    return (Just x)
    Nothing     -> return Nothing

