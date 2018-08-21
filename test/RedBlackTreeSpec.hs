module RedBlackTreeSpec where

import           Data.List             as List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           RedBlackTree          (Color (..), RBTree (..))
import qualified RedBlackTree          as RBT

import           Debug.Trace

spec :: Spec
spec =
  describe "Red Black Tree" $ do
    prop "the root is black" $ \list ->
      (not . null $ (list :: [Int])) ==>
        let (Node c _ _ _) = foldl' RBT.insert Empty list
        in c `shouldBe` B

    prop "if a node is red, then both its children are black" $ \list ->
      (not . null $ (list :: [Int])) ==>
        let reds = redNodes $ foldl' RBT.insert Empty list
            redNodes :: Ord a => RBTree a -> [RBTree a]
            redNodes Empty = []
            redNodes node@(Node _ r _ l) = if isRed node then [node] else [] ++ redNodes r ++ redNodes l
            isRed (Node c _ _ _) = c == R
            isRed Empty          = False
        in
           reds `shouldSatisfy` all (\(Node _ l _ r) -> not (isRed l) && not (isRed r))
