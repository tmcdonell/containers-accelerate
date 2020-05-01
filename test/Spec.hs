
module Main where

import HashMap
import Sort.Quick

-- import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.LLVM.Native

import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup "containers-accelerate"
    [ test_quicksort runN
    , test_hashmap runN
    ]

