
module YaLedger.Tests.Instances where

import Control.Applicative
import Control.Monad
import Data.Decimal
import Data.Dates
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Arbitrary

import YaLedger.Types

instance Arbitrary DateTime where
  arbitrary = DateTime 
               <$> elements [1900..3000]
               <*> elements [1..12]
               <*> elements [1..31]
               <*> elements [0..23]
               <*> elements [0..59]
               <*> elements [0..59]

instance Arbitrary a => Arbitrary (Ext a) where
  arbitrary = Ext <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Amount where
  arbitrary = (:#) <$> arbitrary <*> elements ["р", "€", "$", "¥", ""]

instance Arbitrary Param where
  arbitrary = oneof [fixed, param, plus]
    where
      fixed = Fixed <$> arbitrary
      param = Param <$> elements [1..100] <*> arbitrary <*> arbitrary
      plus = Plus <$> arbitrary <*> arbitrary

instance Arbitrary v => Arbitrary (Posting v Credit) where
  arbitrary = DPosting 
