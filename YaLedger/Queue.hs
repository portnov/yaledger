
module YaLedger.Queue where

import Control.Concurrent.STM
import qualified Data.PQueue.Prio.Min as Q

import YaLedger.Types.Common

newQueue :: IO (Queue a)
newQueue = newTVarIO Q.empty

enqueue :: Integer -> Ext a -> Queue a -> STM ()
enqueue i x queue =
  modifyTVar queue $
    \q -> Q.insert (getDate x, i) x q
  
getFromQueue :: Queue a -> STM (Maybe (Integer, Ext a))
getFromQueue queue = do
  q <- readTVar queue
  case Q.minViewWithKey q of
    Just (((_,i),x), q') -> do
                    writeTVar queue q'
                    return $ Just (i, x)
    Nothing     -> return Nothing

