module InsertionSort
  ( insertion
  ) where

insert :: Ord a => [a] -> a -> [a]
insert [] x = [x]
insert (y:ys) x =
  if x < y
    then x : y : ys
    else y : insert ys x

insertion :: Ord a => [a] -> [a]
insertion = foldl insert []
