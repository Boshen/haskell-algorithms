module InsertionSortSpec where

import           Data.List
import           Test.QuickCheck
import           Test.Hspec

import qualified InsertionSort as Sort

spec :: Spec
spec = describe "Insertion Sort" $
  specify "test" $ property $ \list ->
    Sort.insertion (list :: [Int]) `shouldBe` sort list
