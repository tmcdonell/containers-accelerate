{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Tree.Radix
-- Copyright   : [2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Radix tree (Patricia tree) construction, based on the paper "Maximising
-- Parallelism in Construction of BVHs, Octrees, and k-d Trees", Tero
-- Karras, in High Performance Graphics (2012).
--

module Data.Array.Accelerate.Data.Tree.Radix
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Bits

import qualified Data.Bits as P
import qualified Prelude   as P


data Node = Node !Ptr   -- left pointer
                 !Ptr   -- right pointer
                 !Int   -- parent pointer
  deriving (Show, Generic, Elt, IsProduct Elt)

pattern Node_ :: Exp Ptr -> Exp Ptr -> Exp Int -> Exp Node
pattern Node_ l r p = Pattern (l, r, p)
{-# COMPLETE Node_ #-}

-- If the MSB is set, then this is a leaf pointer. This is fine because who
-- uses signed integers for array indices anyway?? ¯\_(ツ)_/¯
--
newtype Ptr = Ptr Int
  deriving (Generic, Elt, IsProduct Elt)

instance Show Ptr where
  showsPrec d (Ptr x)
    = P.showParen (d P.> 10)
    $ case P.testBit x (P.finiteBitSize (undefined :: Key) - 1) of
        True  -> P.showString "Leaf "  . P.showsPrec 11 (P.clearBit x (P.finiteBitSize (undefined :: Key) - 1))
        False -> P.showString "Inner " . P.showsPrec 11 x

pattern Ptr_ :: Exp Int -> Exp Ptr
pattern Ptr_ x = Pattern x
{-# COMPLETE Ptr_ #-}

type Key = Int

-- Construct the binary radix tree from the vector of keys. The keys must
-- be sorted.
--
binary_radix_tree :: Acc (Vector Key) -> Acc (Vector Node)
binary_radix_tree keys = zipWith3 Node_ lefts rights parents
  where
    n = length keys

    delta i j =
      if j >= 0 && j < n
         then
          let li = keys !! i
              lj = keys !! j
              -- handle duplicates using the index as a tiebreaker if
              -- necessary
           in if li == lj
                 then 32 + countLeadingZeros (i  `xor` j)
                 else      countLeadingZeros (li `xor` lj)
         else -1

    node i =
      let -- determine direction of the range
          d = signum $ delta i (i+1) - delta i (i-1)

          -- compute upper bound for the length of the range
          delta_min = delta i (i-d)
          l_max     = while (\l_max' -> delta i (i+l_max'*d) > delta_min)
                            (*4)    -- *2
                            128     -- 2

          -- find the other end using binary search
          T2 l _ = while (\(T2 _  t) -> t > 0)
                         (\(T2 l' t) ->
                            let t2 = t `quot` 2 in
                            if delta i (i+(l'+t) * d) > delta_min
                               then T2 (l' + t) t2
                               else T2 l'       t2)
                         (T2 0 (l_max `quot` 2))
          j      = i + l*d

          -- find the split position using binary search
          delta_node = delta i j
          T2 s _     = while (\(T2 _  q) -> q <= l)
                             (\(T2 s' q) ->
                               let r = q*2
                                   t = (l + r - 1) `quot` r
                                in if delta i (i+(s'+t)*d) > delta_node
                                      then T2 (s'+t) r
                                      else T2 s'     r)
                             (T2 0 1)
          gamma = i + s*d + min d 0

          -- output child pointers
          T2 left left_parent =
            if min i j == gamma
               then T2 (leaf  gamma) (-1)
               else T2 (inner gamma) gamma

          T2 right right_parent =
            if max i j == gamma + 1
               then T2 (leaf  (gamma+1)) (-1)
               else T2 (inner (gamma+1)) (gamma+1)

          leaf  x = Ptr_ (setBit x (finiteBitSize (undef @Key) - 1))
          inner x = Ptr_ x
      in
      T4 left right
         (T2 left_parent  i)
         (T2 right_parent i)

    (lefts, rights, parents_a, parents_b) = unzip4 $ generate (I1 (n-1)) (node . unindex1)

    parents = let vals = map snd parents_a ++ map snd parents_b
                  dest = map fst parents_a ++ map fst parents_b
               in permute const
                    (fill (I1 (n-1)) undef)
                    (\ix -> let d = dest ! ix in
                             if d < 0 then ignore else I1 d)
                    vals

