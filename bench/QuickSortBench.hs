import           Criterion.Main

import qualified QuickSort      as Sort
import           System.Random

size = 5000000

main :: IO ()
main = do
  gen <- getStdGen
  let nums = take size $ randoms gen :: [Int]
  defaultMain
    [ bgroup
        "quick"
        [ bench "quick sort" $ whnf Sort.quick nums
        , bench "acummulated quick sort" $ whnf Sort.asort nums
        , bench "merge sort" $ whnf Sort.msort nums
        ]
    ]
