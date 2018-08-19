module BinarySearchTree where

import qualified Data.List as List
import           Prelude   hiding (length, lookup, maximum, minimum)

data Tree a
  = Empty
  | Node (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert k Empty = Node Empty k Empty
insert k (Node l x r)
  | k < x = Node (insert k l) x r
  | otherwise = Node l x (insert k r)

fromList :: Ord a => [a] -> Tree a
fromList = List.foldl' (flip insert) Empty

toList :: Tree a -> [a]
toList Empty        = []
toList (Node l x r) = toList l ++ [x] ++ toList r

preorder :: Tree a -> [a]
preorder (Node l x r) = [x] ++ preorder l ++ preorder r
preorder Empty        = []

inorder :: Tree a -> [a]
inorder = toList

postorder :: Tree a -> [a]
postorder (Node l x r) = postorder l ++ postorder r ++ [x]
postorder Empty        = []

lookup :: Ord a => a -> Tree a -> Tree a
lookup _ Empty = Empty
lookup k n@(Node l x r)
  | k == x = n
  | k < x = lookup k l
  | otherwise = lookup k r

minimum :: Tree a -> a
minimum Empty            = error "empty tree"
minimum (Node Empty x _) = x
minimum (Node l x _)     = minimum l

maximum :: Tree a -> a
maximum Empty            = error "empty tree"
maximum (Node _ x Empty) = x
maximum (Node _ x r)     = maximum r

delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete x (Node l k r)
  | x < k = Node (delete x l) k r
  | x > k = Node l k (delete x r)
  | isEmpty l = r
  | isEmpty r = l
  | otherwise =
    let k' = minimum r
     in Node l k' (delete k' r)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

length :: Tree a -> Int
length Empty        = 0
length (Node l x r) = length l + 1 + length r
