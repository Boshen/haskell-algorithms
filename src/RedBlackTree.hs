{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RedBlackTree where

-- Red and Black Tree
-- 1. Every node is either red or black.
-- 2. The root is black.
-- 3. Every leaf (NIL) is black.
-- 4. If a node is red, then both its children are black.
-- 5. For each node, all paths from the node to descendant leaves contain the same number of black nodes.
-- p.s. a leaf (NIL) node is not the node with a value, it's the node's children.
data Color
  = R
  | B
  deriving (Eq, Show)

data RBTree a
  = Empty
  | Node Color
         (RBTree a)
         a
         (RBTree a)
  deriving (Eq, Show)

insert :: Ord a => RBTree a -> a -> RBTree a
insert t x = makeBlack $ ins t
  where
    ins Empty = Node R Empty x Empty
    ins (Node color l k r)
      | x < k = balance color (ins l) k r
      | otherwise = balance color l k (ins r)
    makeBlack (Node _ l k r) = Node B l k r

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (Node R (Node R a x b) y c) z d =
  Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d =
  Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) =
  Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) =
  Node R (Node B a x b) y (Node B c z d)
balance color l k r = Node color l k r
