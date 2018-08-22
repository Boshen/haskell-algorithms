import           Criterion.Main

import qualified QuickSort      as Sort
import           System.Random

size = 1000000

main :: IO ()
main = do
  gen <- getStdGen
  let nums = take size $ randoms gen :: [Int]
  defaultMain
    [ bgroup
        "quick"
        [ bench "p1" $ whnf Sort.quick nums
        , bench "p2" $ whnf Sort.quick2 nums
        ]
    ]
