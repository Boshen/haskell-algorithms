module QuickSort where

import qualified Data.List as List

quick :: Ord a => [a] -> [a]
quick [] = []
quick (x:xs) = quick as ++ [x] ++ quick bs
  where (as, bs) = List.partition (<= x) xs

quick2 :: Ord a => [a] -> [a]
quick2 [] = []
quick2 (x:xs) = quick as ++ [x] ++ quick bs
  where (as, bs) = partition (<= x) xs

partition _ [] = ([], [])
partition p (x:xs) = let (as, bs) = partition p xs in
    if p x then (x:as, bs) else (as, x:bs)
