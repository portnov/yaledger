{-# LANGUAGE GADTs, RecordWildCards, FlexibleContexts #-}
module YaLedger.Processor.Rules
  (runRules) where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.Decimal
import Data.Hashable
import qualified Data.Map as M
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel
import YaLedger.Kernel.Correspondence
import YaLedger.Processor.Templates

-- | Check if posting matches to 'Condition'
matchC :: Throws NoSuchRate l
       => M.Map AccountID [GroupID] -- ^ Precomputed map of account groups
       -> DateTime            -- ^ Posting date/time
       -> Posting Decimal t   -- ^ Posting itself
       -> Condition
       -> Atomic l Bool
matchC groupsMap date (CPosting acc x _) cond = check groupsMap ECredit date acc x cond
matchC groupsMap date (DPosting acc x _) cond = check groupsMap EDebit  date acc x cond

check groupsMap t date acc x (Condition {..}) = do
  let accID = getID acc
  let grps = fromMaybe [] $ M.lookup accID groupsMap
      action = maybe [ECredit, EDebit] (\x -> [x]) cAction
  if (t `elem` action) &&
     ((accID `elem` cAccounts) ||
      (any (`elem` cGroups) grps))
    then do
         let accountCurrency = getCurrency acc
         if cValue == AnyValue
           then return True
           else do
                let (op, v) = case cValue of
                                MoreThan s -> ((>), s)
                                LessThan s -> ((<), s)
                                Equals s   -> ((==), s)
                                _ -> error "Processor.Rules.matchC.check: Impossible."
                condValue :# _ <- convert (Just date) accountCurrency v
                return $ x `op` condValue
    else return False

-- | Apply given function to all transactions which are
-- produced by all rules, applicable for this posting
runRules :: (Throws NoSuchRate l,
             Throws NoCorrespondingAccountFound l,
             Throws InvalidAccountType l,
             Throws NoSuchTemplate l,
             Throws InternalError l)
         => PostingType
         -> DateTime                                   -- ^ Date/time of the posting
         -> Integer
         -> Attributes                                 -- ^ Posting attributes
         -> Posting Decimal t                          -- ^ Posting itself
         -> (Integer -> Ext (Transaction Amount) -> Atomic l ())  -- ^ Function to apply to produced transactions
         -> Atomic l ()
runRules pt date tranID pAttrs p run = do
  rules <- gets (case pt of
                   ECredit -> creditRules . lsRules
                   EDebit  -> debitRules  . lsRules )
  groupsMap <- gets lsFullGroupsMap
  let ns = enumFrom (tranID + 1)
  forM_ (zip ns rules) $ \(i, (name, attrs, When cond tran)) -> do
    y <- matchC groupsMap date p cond
    if y && (pAttrs `matchAll` cAttributes cond)
      then do
           let attrs' = M.insert "rule" (Exactly name) (pAttrs `M.union` attrs)
               (c, x) = case p of
                            DPosting acc x _ -> (getCurrency acc, x)
                            CPosting acc x _ -> (getCurrency acc, x)
           infoSTM $ "Firing rule " ++ name
           tran' <- liftTemplate $ fillTemplate tran [x :# c]
           let pos = newPos ("<generated by rule \"" ++ name ++ "\">") 0 0
           run i (Ext date (fromIntegral $ hash p) pos attrs' tran')
      else return ()

