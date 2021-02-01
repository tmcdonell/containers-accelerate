
module Main where

import HashMap
import Sort.Merge
import Sort.Quick

-- import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.LLVM.Native

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Hedgehog

main :: IO ()
main
  = defaultMain
  $ localOption (NumThreads 1)                        -- run each test sequentially with many cores
  $ localOption (HedgehogTestLimit (Just 100))        -- number of each test to run
  $ testGroup "containers-accelerate"
    [ test_quicksort runN
    -- , test_mergesort runN
    , test_hashmap runN
    ]

