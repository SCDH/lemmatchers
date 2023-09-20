module Lemmatchers.TagRecordsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = prop "dummy-property" $ \i -> i > 0 ==> abs i === (i :: Int)
