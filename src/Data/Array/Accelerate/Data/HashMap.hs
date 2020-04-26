{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Array.Accelerate.Data.HashMap
-- Copyright   : [2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.HashMap (

  HashMap,

  -- * Basic interface
  size,
  member,
  lookup,
  -- alter

  -- * Transformations
  map,
  mapWithKey,

  -- * Conversions
  keys,
  elems,

  -- * Arrays
  fromVector,
  toVector,

) where

import Data.Array.Accelerate                              hiding ( size, map )
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Data.Maybe
import qualified Data.Array.Accelerate                    as A

import Data.Array.Accelerate.Data.Hashable
import Data.Array.Accelerate.Data.Tree.Radix
import Data.Array.Accelerate.Data.Sort.Quick

import Data.Function


-- | A map from keys to values. The map can not contain duplicate keys.
--
data HashMap k v = HashMap (Scalar Int) (Vector Node) (Vector (k,v))
  deriving (Show, Generic, Arrays)

pattern HashMap_
    :: (Elt k, Elt v)
    => Acc (Scalar Int)     -- key bits
    -> Acc (Vector Node)    -- tree structure
    -> Acc (Vector (k,v))   -- (key,value) pairs
    -> Acc (HashMap k v)
pattern HashMap_ h t kv = Pattern (h,t,kv)
{-# COMPLETE HashMap_ #-}


-- | /O(1)/ Return the number of key-value mappings
--
size :: (Elt k, Elt v) => Acc (HashMap k v) -> Exp Int
size (HashMap_ _ _ kv) = length kv

-- | /O(k)/ Return 'True' if the specified key is present in the map,
-- 'False' otherwise
--
member :: (Eq k, Hashable k, Elt v) => Exp k -> Acc (HashMap k v) -> Exp Bool
member k m =
  if isJust (lookup k m)
     then True_
     else False_

-- | /O(k)/ Return the value to which the specified key is mapped, or
-- 'Nothing' if the map contains no mapping for the key.
--
lookup :: (Eq k, Hashable k, Elt v) => Exp k -> Acc (HashMap k v) -> Exp (Maybe v)
lookup k hm = snd `fmap` lookupWithIndex k hm

lookupWithIndex :: (Eq k, Hashable k, Elt v) => Exp k -> Acc (HashMap k v) -> Exp (Maybe (Int, v))
lookupWithIndex key (HashMap_ msb tree kv) = result
  where
    h                 = hash key
    bits              = finiteBitSize (undef @Key)
    index  (Ptr_ x)   = clearBit x (bits - 1)
    isLeaf (Ptr_ x)   = testBit  x (bits - 1)

    Node_ l0 r0 _     = tree !! 0
    b0                = the msb
    step (T4 b l r a) =
      let p = testBit h (bits - b) ? (r, l)
          i = index p
       in if isLeaf p
             then let T2 k v = kv !! i
                   in T4 (bits+1) undef undef (k == key ? (Just_ (T2 i v), Nothing_))
             else let Node_ l' r' _ = tree !! i
                   in T4 (b+1) l' r' a

    -- If the map contains fewer than two elements, the radix tree will be
    -- empty. Otherwise, we recurse based on the next differing bit of the
    -- internal pointers.
    --
    result  = length kv == 0 ? ( Nothing_
            , length kv == 1 ? ( let T2 k v = kv !! 0
                                  in k == key ? (Just_ (T2 0 v), Nothing_)
            , {- otherwise -}  ( let T4 _ _ _ mv = while (\(T4 b _ _ _) -> b <= bits)
                                                         step
                                                         (T4 b0 l0 r0 Nothing_)
                                  in mv)))

-- | /O(n)/ Transform the map by applying a function to every value
--
map :: (Elt k, Elt v1, Elt v2) => (Exp v1 -> Exp v2) -> Acc (HashMap k v1) -> Acc (HashMap k v2)
map f = mapWithKey (const f)

-- | /O(n)/ Transform this map by applying a function to every value
--
mapWithKey :: (Elt k, Elt v1, Elt v2) => (Exp k -> Exp v1 -> Exp v2) -> Acc (HashMap k v1) -> Acc (HashMap k v2)
mapWithKey f (HashMap_ b t kv)
  = HashMap_ b t
  $ A.map (\(T2 k v) -> T2 k (f k v)) kv

-- | /O(1)/ Return this map's keys
--
keys :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector k)
keys (HashMap_ _ _ kv) = A.map fst kv

-- | /O(1)/ Return this map's values
--
elems :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector v)
elems (HashMap_ _ _ kv) = A.map snd kv

-- | /O(n log n)/ Construct a map from the supplied (key,value) pairs
--
fromVector :: (Hashable k, Elt v) => Acc (Vector (k,v)) -> Acc (HashMap k v)
fromVector assocs = HashMap_ msb tree kv
  where
    (h, kv) = unzip
            . quicksortBy (compare `on` fst)
            $ A.map (\(T2 k v) -> T2 (hash k) (T2 k v)) assocs

    tree          = binary_radix_tree h
    Node_ l0 _ _  = tree !! 0
    msb           = unit $ countLeadingZeros (h !! index l0)

    bits            = finiteBitSize (undef @Key)
    index  (Ptr_ x) = clearBit x (bits - 1)

-- | /O(1)/ Return this map's (key,value) pairs
--
toVector :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector (k,v))
toVector (HashMap_ _ _ kv) = kv

