module InsertionSortSpec where

import           Data.List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified InsertionSort         as Sort

spec :: Spec
spec = describe "Insertion Sort" $
  prop "test" $ \list ->
    Sort.insertion (list :: [Int]) `shouldBe` sort list
