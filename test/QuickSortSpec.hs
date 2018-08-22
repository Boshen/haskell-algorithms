module QuickSortSpec where

import qualified Data.List             as List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified QuickSort             as Sort

spec :: Spec
spec = describe "Quick Sort" $ do
  describe "Quick Sort" $ do
    prop "quick" $ \list ->
      Sort.quick (list :: [Int]) `shouldBe` List.sort list

    prop "merge" $ \list ->
      Sort.msort (list :: [Int]) `shouldBe` List.sort list
