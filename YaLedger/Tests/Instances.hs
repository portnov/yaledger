{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module YaLedger.Tests.Instances where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import qualified Data.Map as M
import Data.Decimal
import Data.Dates

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Arbitrary

import YaLedger.Types
import YaLedger.Exceptions

instance Arbitrary DateTime where
  arbitrary = DateTime 
               <$> elements [1900..3000]
               <*> elements [1..12]
               <*> elements [1..31]
               <*> elements [0..23]
               <*> elements [0..59]
               <*> elements [0..59]

instance Arbitrary Currency where
  arbitrary = Currency
                <$> elements ["р", "€", "$", "¥", ""]
                <*> return Nothing
                <*> return Nothing
                <*> elements [2,3,4]

instance Arbitrary Attributes where
  arbitrary = M.fromList <$> arbitrary

instance Arbitrary AttributeValue where
  arbitrary = do
    exact <- Exactly <$> elements ["exactly", "nothing", "test"]
    option <- Optional <$> elements ["optional", "nothing", "test"]
    oneOf <- OneOf <$> arbitrary
    anybut <- AnyBut <$>  elements ["optional", "nothing", "test"]
    elements [exact, option, oneOf, anybut, Any]

instance Arbitrary a => Arbitrary (Ext a) where
  arbitrary = Ext <$> arbitrary <*> return 0 <*> return nowhere <*> arbitrary <*> arbitrary

instance Arbitrary Amount where
  arbitrary = (:#) <$> arbitrary <*> arbitrary

instance Arbitrary Param where
  arbitrary = oneof [fixed, param, plus]
    where
      fixed = Fixed <$> arbitrary
      param = Param <$> elements [1..100] <*> arbitrary <*> arbitrary
      plus = Plus <$> arbitrary <*> arbitrary

data TestFailed = TestFailed String String SourcePos
  deriving (Typeable)

instance Show TestFailed where
  show (TestFailed name msg pos) =
    showPos pos $ 
      "Test `" ++ name ++ "' failed:\n    " ++ msg

instance Exception TestFailed

failTest name message = do
  throwP (TestFailed name message)

runTest name test = do
    test
    putStrLn $ "Test passed: " ++ name

