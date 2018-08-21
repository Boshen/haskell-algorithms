module BinarySearchTreeSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           BinarySearchTree      (Tree (..))
import qualified BinarySearchTree      as T

spec :: Spec
spec = describe "binary search tree" $ do
  describe "insert" $ do
    prop "empty" $ \x ->
      T.insert (x :: Int) Empty == Node Empty x Empty

    prop "left" $ \x ->
      let node = T.insert 1 Empty
      in x < 1 ==> T.insert (x :: Int) node == Node (Node Empty x Empty) 1 Empty

    prop "right" $ \x ->
      let node = T.insert 1 Empty
      in x >= 1 ==> T.insert (x :: Int) node == Node Empty 1 (Node Empty x Empty)

  specify "fromList" $
    T.fromList [1, 2] `shouldBe` Node Empty 1 (Node Empty 2 Empty)

  specify "toList" $
    T.toList (Node (Node Empty 1 Empty) 2 Empty) `shouldBe` [1, 2]

  specify "preorder" $
    T.preorder tree `shouldBe` [4, 3, 1, 2, 8, 7, 16, 10, 9, 14]

  specify "inorder" $
    T.inorder tree `shouldBe` [1, 2, 3, 4, 7, 8, 9, 10, 14, 16]

  specify "postorder" $
    T.postorder tree `shouldBe` [2, 1, 3, 7, 9, 14, 10, 16, 8, 4]

  specify "lookup" $ do
    T.lookup 0 Empty `shouldBe` Empty
    T.lookup 14 tree `shouldBe` Node Empty 14 Empty
    T.lookup 9 tree `shouldBe` Node Empty 9 Empty

  prop "maximum" $ \list ->
    length list /= 0 ==> T.maximum (T.fromList list) == maximum (list :: [Int])

  prop "minimum" $ \list ->
    length list /= 0 ==> T.minimum (T.fromList list) == minimum (list :: [Int])

  specify "delete" $ do
    T.delete 0 Empty `shouldBe` Empty
    T.delete 3 (T.fromList [2, 1, 3]) `shouldBe` Node (Node Empty 1 Empty) 2 Empty
    T.delete 1 (T.fromList [2, 1, 3]) `shouldBe` Node Empty 2 (Node Empty 3 Empty)
    T.delete 2 (T.fromList [2, 1, 3]) `shouldBe` Node (Node Empty 1 Empty) 3 Empty

  prop "length" $ \list ->
    T.length (T.fromList list) == length (list :: [Int])

tree :: Tree Int
tree = T.fromList [4, 3, 1, 2, 8, 7, 16, 10, 9, 14]
