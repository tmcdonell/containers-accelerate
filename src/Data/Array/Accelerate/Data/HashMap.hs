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

  HashMap, Hashable,

  -- * Construction
  fromVector,

  -- * Basic interface
  size,
  member,
  lookup,
  insert, insertWith, insertWithKey,
  delete,
  adjust, adjustWithKey,

  -- * Transformations
  map,
  mapWithKey,

  -- * Conversions
  keys,
  elems,
  assocs,

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
data HashMap k v = HashMap (Vector Node) (Vector (k,v))
  deriving (Show, Generic, Arrays)

pattern HashMap_
    :: (Elt k, Elt v)
    => Acc (Vector Node)    -- tree structure
    -> Acc (Vector (k,v))   -- (key,value) pairs
    -> Acc (HashMap k v)
pattern HashMap_ t kv = Pattern (t,kv)
{-# COMPLETE HashMap_ #-}


-- | /O(1)/ Return the number of key-value mappings
--
size :: (Elt k, Elt v) => Acc (HashMap k v) -> Exp Int
size (HashMap_ _ kv) = length kv

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
lookupWithIndex key (HashMap_ tree kv) = result
  where
    h                 = hash key
    n                 = length tree
    bits              = finiteBitSize (undef @Key)
    index  (Ptr_ x)   = clearBit x (bits - 1)
    isLeaf (Ptr_ x)   = testBit  x (bits - 1)

    result =
      if length kv < 2
         then if length kv == 0
                 then Nothing_                -- empty map!
                 else let T2 k v = kv !! 0    -- the tree structure is empty
                       in k == key ? (Just_ (T2 0 v), Nothing_)
         else
           snd $ while (\(T2 i _) -> i < n) search (T2 0 Nothing_)

    search (T2 i _) =
      let Node_ d l r p = tree !! i
          d'            = fromIntegral d
       in if d' < bits
             then let m = testBit h (bits - d' - 1) ? (r, l)
                      j = index m
                   in if isLeaf m
                         then let T2 k v = kv !! j
                               in T2 n (k == key ? (Just_ (T2 j v), Nothing_))
                         else T2 j Nothing_
             else
               -- there was a hash collision; exhaustively search this
               -- sub-tree comparing the keys
               let T3 _ _ x = while (\(T3 j _ c) -> isNothing c && j /= p)
                                    exhaust
                                    (T3 i (-1) Nothing_)
                in T2 n x

    exhaust (T3 i prev _) =
      let Node_ _ l r p = tree !! i
          fromLeft      = index l == prev
          fromRight     = index r == prev
       in if fromLeft
             then -- recurse right
               let j = index r
               in if isLeaf r
                     then let T2 k v = kv !! j
                           in T3 i j (k == key ? (Just_ (T2 j v), Nothing_))
                     else T3 j i Nothing_
             else
          if fromRight
             then -- go up to the parent
               T3 p i Nothing_
             else -- recurse left
               let j = index l
                in if isLeaf l
                      then let T2 k v = kv !! j
                            in T3 i j (k == key ? (Just_ (T2 j v), Nothing_))
                      else T3 j i Nothing_


-- | Insert new (key,value) pairs into the map. If the key is already
-- present in the map, the associated value is replaced with the supplied
-- value.
--
insert :: (Eq k, Hashable k, Elt v)
       => Acc (Vector (k,v))
       -> Acc (HashMap k v)
       -> Acc (HashMap k v)
insert = insertWith const

-- | Insert with a function combining the new value and old value. Each
-- pair will be inserted into the map if the key does not already exist. If
-- the key exists, the pair '(key, f new_value old_value)' will be
-- inserted.
--
insertWith
    :: (Eq k, Hashable k, Elt v)
    => (Exp v -> Exp v -> Exp v)
    -> Acc (Vector (k,v))
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
insertWith f = insertWithKey (const f)

-- | /O(n log n)/ Insert values into the map using a function to combine
-- the new value and old value. Each pair will be inserted into the map if
-- the key does not already exist. If the key exists, the pair
-- '(key, f key new_value old_value)' will be inserted.
--
insertWithKey
    :: (Eq k, Hashable k, Elt v)
    => (Exp k -> Exp v -> Exp v -> Exp v)
    -> Acc (Vector (k,v))
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
insertWithKey f kv hm@(HashMap_ tree kv0) =
  let
      -- TODO: This is very inefficient. We should update the existing
      -- association array in-place to keep that part in sorted order, and
      -- then sort and merge in the new key-value pairs before recreating
      -- the tree structure. This should be quicker than sorting the entire
      -- combined association array.
      --
      -- TODO: Handle inputs containing non-unique keys

      -- Update the values of any keys which already exist in the map
      -- in-place. This keeps the main association array in sorted order
      --
      old        = if the sz == length kv  -- no existing values were updated
                     then kv0
                     else permute const kv0 (\ix -> let i = is ! ix in i < 0 ? (Nothing_, Just_ (I1 i))) kv'
      (is, kv') = unzip
                $ A.map (\(T2 k v) -> let mu = lookupWithIndex k hm
                                       in if isJust mu
                                             then let T2 i u = fromJust mu
                                                   in T2 i (T2 k (f k v u))
                                             else T2 (-1) undef) kv

      -- Any keys which were not already in the map now need to be added
      --
      T2 new sz = filter (\(T2 i _) -> i < 0)
                $ zip is kv
   in
   if the sz == 0
      then HashMap_ tree old
      else fromVector (old ++ A.map snd new)    -- TODO: merge sorted subarrays & rebuild tree


-- | Delete a key and its value from the map. When the key is not a member
-- of the map, that key is ignored
--
delete :: (Eq k, Hashable k, Elt v)
       => Acc (Vector k)
       -> Acc (HashMap k v)
       -> Acc (HashMap k v)
delete ks hm =
  let
      -- determine indices of the association array which need to be removed
      T2 is sz = justs
               $ A.map (\k -> let mu = lookupWithIndex k hm
                               in if isJust mu
                                     then let T2 i _ = fromJust mu
                                           in Just_ i
                                     else Nothing_) ks

      -- the (key,value) pairs are still in sorted order after knocking out
      -- the deleted elements, so we can recreate the tree directly
      T2 kv' _ = justs
               . scatter is (A.map Just_ (assocs hm))
               $ fill (shape is) Nothing_
      h'       = A.map (bitcast . hash . fst) kv'
      tree'    = binary_radix_tree h'
   in
   if the sz == 0
      then hm
      else HashMap_ tree' kv'


-- | Update a value at a specific key using the provided function. When the
-- key is not a member of the map, that key is ignored.
--
adjust :: (Eq k, Hashable k, Elt v)
       => (Exp v -> Exp v)
       -> Acc (Vector k)
       -> Acc (HashMap k v)
       -> Acc (HashMap k v)
adjust f = adjustWithKey (const f)

-- | Update a value at a specific key using the provided function. When the
-- key is not a member of the map, that key is ignored.
--
adjustWithKey
    :: (Eq k, Hashable k, Elt v)
    => (Exp k -> Exp v -> Exp v)
    -> Acc (Vector k)
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
adjustWithKey f ks hm@(HashMap_ tree kvs) =
  let
      (is, new) = unzip iv
      T2 iv sz  = justs
                $ A.map (\k -> let mv = lookupWithIndex k hm
                                in if isJust mv
                                      then let T2 i v = fromJust mv
                                            in Just_ (T2 i (T2 k (f k v)))
                                      else Nothing_) ks
   in
   if the sz == 0
      then hm
      else HashMap_ tree (scatter is kvs new)


-- | /O(n)/ Transform the map by applying a function to every value
--
map :: (Elt k, Elt v1, Elt v2) => (Exp v1 -> Exp v2) -> Acc (HashMap k v1) -> Acc (HashMap k v2)
map f = mapWithKey (const f)

-- | /O(n)/ Transform this map by applying a function to every value
--
mapWithKey :: (Elt k, Elt v1, Elt v2) => (Exp k -> Exp v1 -> Exp v2) -> Acc (HashMap k v1) -> Acc (HashMap k v2)
mapWithKey f (HashMap_ t kv)
  = HashMap_ t
  $ A.map (\(T2 k v) -> T2 k (f k v)) kv

-- | /O(1)/ Return this map's keys
--
keys :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector k)
keys (HashMap_ _ kv) = A.map fst kv

-- | /O(1)/ Return this map's values
--
elems :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector v)
elems (HashMap_ _ kv) = A.map snd kv

-- | /O(1)/ Return this map's (key,value) pairs
--
assocs :: (Elt k, Elt v) => Acc (HashMap k v) -> Acc (Vector (k,v))
assocs (HashMap_ _ kv) = kv

-- | /O(n log n)/ Construct a map from the supplied (key,value) pairs
--
fromVector :: (Hashable k, Elt v) => Acc (Vector (k,v)) -> Acc (HashMap k v)
fromVector v = HashMap_ tree kv
  where
    tree    = binary_radix_tree h
    kv      = gather p v
    (h, p)  = unzip
            . sortBy (compare `on` fst)
            $ imap (\(I1 i) (T2 k _) -> T2 (bitcast (hash k)) i) v

