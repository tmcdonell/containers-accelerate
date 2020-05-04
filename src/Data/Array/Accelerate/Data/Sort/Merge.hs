{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Sort.Merge
-- Copyright   : [2020] Ivo Gabe de Wolff, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.Sort.Merge (

  sort,
  sortBy,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe


-- | A stable merge sort. This is a special case of 'sortBy' which allows
-- the user to supply their own comparison function.
--
sort :: Ord a => Acc (Vector a) -> Acc (Vector a)
sort = sortBy compare

-- | A non-overloaded version of 'sort'
--
-- It is often convenient to use this together with 'Data.Function.on', for
-- instance: 'sortBy' ('compare' `on` 'fst').
--
sortBy :: Elt a => (Exp a -> Exp a -> Exp Ordering) -> Acc (Vector a) -> Acc (Vector a)
sortBy cmp input = output
  where
    n           = length input
    T2 _ output = awhile condition step
                    (T2 (unit insertion_segment_size) (insertion_sort cmp n input))

    condition (T2 blockSize _)  = map (< length input) blockSize
    step (T2 blockSize' values) = T2 (unit $ blockSize * 2) values'
      where
        blockSize  = the blockSize'
        newIndices = imap (newIndex values cmp n blockSize) values
        values'    = scatter newIndices (fill (index1 n) undef) values


insertion_segment_size :: Exp Int
insertion_segment_size = 32

insertion_sort
    :: Elt a
    => (Exp a -> Exp a -> Exp Ordering)
    -> Exp Int
    -> Acc (Vector a)
    -> Acc (Vector a)
insertion_sort cmp n xs = scatter indices (fill (I1 n) undef) xs
  where
    indices     = imap f xs
    f (I1 ix) x = segment_start + offset
      where
        segment           = ix `quot` insertion_segment_size
        segment_start     = segment * insertion_segment_size
        segment_end       = ((segment + 1) * insertion_segment_size) `min` n
        T2 _ offset       = while
          (\(T2 i _) -> i < segment_end)
          (\(T2 i c) ->
              let x'      = xs !! i
                  smaller = let d = cmp x' x
                             in d == LT_ || d == EQ_ && i > ix
               in
               T2 (i + 1) (c + (smaller ? (1, 0))))
          (T2 segment_start 0)

newIndex
    :: Elt a
    => Acc (Vector a)
    -> (Exp a -> Exp a -> Exp Ordering)
    -> Exp Int
    -> Exp Int
    -> Exp DIM1
    -> Exp a
    -> Exp Int
newIndex values cmp valueCount blockSize (I1 index) value = index + offset
  where
    blockIndex      = index `quot` blockSize
    left            = even blockIndex
    otherBlockIndex = blockIndex + (left ? (1, -1))

    searchMinIndex  = otherBlockIndex * blockSize
    searchMaxIndex  = min valueCount $ (otherBlockIndex + 1) * blockSize

    countOtherBlock = binarySearch values cmp value (not left) searchMinIndex searchMaxIndex

    -- We should base the indices of the right block also on the left
    -- block, hence we must subtract blockSize
    --
    offset = countOtherBlock - (left ? (0, blockSize))

-- Returns the number of elements a_i such that
--   * (a_i <= query) if inclusive is True; or
--   * (a_i <  query) otherwise
-- where initialMinIndex <= i < initialMaxIndex.
--
-- The corresponding section of the input vector must be sorted.
--
binarySearch
    :: Elt a
    => Acc (Vector a)
    -> (Exp a -> Exp a -> Exp Ordering)
    -> Exp a
    -> Exp Bool
    -> Exp Int
    -> Exp Int
    -> Exp Int
binarySearch values cmp query inclusive initialMinIndex initialMaxIndex =
  index - initialMinIndex
  where
    -- The invariant of the loop is a_i `compare` query && not (a_j `compare` query)
    -- where a_initialMaxIndex is treated as infinity and
    -- a_{initialMinIndex - 1} as minus infinity
    --
    T2 _ index = while
      (\(T2 i j) -> i + 1 < j)
      (\(T2 i j) ->
          let m   = (i + j) `quot` 2
              a_m = values !! m
              det = let c = a_m `cmp` query
                     in c == LT_ || c == EQ_ && inclusive
           in
           det ? (T2 m j, T2 i m))
      (T2 (initialMinIndex - 1) initialMaxIndex)

