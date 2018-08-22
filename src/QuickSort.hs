module QuickSort where

import qualified Data.List as List

quick :: Ord a => [a] -> [a]
quick [] = []
quick (x:xs) = quick as ++ [x] ++ quick bs
  where
    (as, bs) = List.partition (<= x) xs

asort :: Ord a => [a] -> [a]
asort xs = asort' xs []

asort' :: Ord a => [a] -> [a] -> [a]
asort' [] acc = acc
asort' (x:xs) acc = asort' as (x : asort' bs acc)
  where
    (as, bs) = part xs [] []
    part [] as bs = (as, bs)
    part (y:ys) as bs
      | y <= x = part ys (y : as) bs
      | otherwise = part ys as (y : bs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
  where
    (as, bs) = splitAt (length xs `div` 2) xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
