{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
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
  empty,
  singleton,
  fromVector,

  -- * Basic interface
  size,
  member,
  lookup, (!?),
  insert, insertWith, insertWithKey,
  delete,
  adjust, adjustWithKey,

  -- * Combination
  union, unionWith, unionWithKey,
  difference, (\\), differenceWith, differenceWithKey,
  intersection, intersectionWith, intersectionWithKey,

  -- * Traversal
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

-- | /O(k)/ Infix version of 'lookup'
--
-- @since 0.2.0.0@
--
(!?) :: (Eq k, Hashable k, Elt v) => Exp k -> Acc (HashMap k v) -> Exp (Maybe v)
(!?) = lookup

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
                $ A.map (\(T2 k v) -> lookupWithIndex k hm & match \case
                                        Just_ (T2 i u) -> T2 i (T2 k (f k v u))
                                        Nothing_       -> T2 (-1) undef) kv

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
               $ A.map (\k -> lookupWithIndex k hm & match \case
                                Just_ (T2 i _) -> Just_ i
                                Nothing_       -> Nothing_) ks

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
                $ A.map (\k -> lookupWithIndex k hm & match \case
                                 Just_ (T2 i v) -> Just_ (T2 i (T2 k (f k v)))
                                 Nothing_       -> Nothing_) ks
   in
   if the sz == 0
      then hm
      else HashMap_ tree (scatter is kvs new)


-- | Left-biased union of two maps
--
-- @since 0.2.0.0@
--
union :: (Eq k, Hashable k, Elt v)
      => Acc (HashMap k v)
      -> Acc (HashMap k v)
      -> Acc (HashMap k v)
union = unionWith const

-- | Union with a combining function
--
-- @since 0.2.0.0@
--
unionWith
    :: (Eq k, Hashable k, Elt v)
    => (Exp v -> Exp v -> Exp v)
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
unionWith f = unionWithKey (const f)

-- | Take the union of two maps with a combining function
--
-- @since 0.2.0.0@
--
unionWithKey
    :: (Eq k, Hashable k, Elt v)
    => (Exp k -> Exp v -> Exp v -> Exp v)
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
    -> Acc (HashMap k v)
unionWithKey f hm1 hm2 = fromVector (kv1 ++ kv2)
  where
    -- Values from the second map which are present in the first
    (kv2, i1) = unzip
              $ A.map (\(T2 k v2) -> lookupWithIndex k hm1 & match \case
                                       Nothing_        -> T2 (T2 k v2)          Nothing_
                                       Just_ (T2 i v1) -> T2 (T2 k (f k v1 v2)) (Just_ (I1 i))) (assocs hm2)

    -- Knock out values from the first map which have already been merged
    -- with values from the second
    kv1       = afst
              $ justs
              $ permute const (A.map Just_ (assocs hm1)) (i1 !) (fill (shape (assocs hm2)) Nothing_)


-- | Left-biased difference of two maps. Returns elements of the first map
-- not existing in the second
--
-- @since 0.2.0.0@
--
difference
    :: (Eq k, Hashable k, Elt a, Elt b)
    => Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k a)
difference = differenceWith (\_ _ -> Nothing_)

-- | Same as 'difference'
--
-- @since 0.2.0.0@
--
infixl 9 \\
(\\) :: (Eq k, Hashable k, Elt a, Elt b) => Acc (HashMap k a) -> Acc (HashMap k b) -> Acc (HashMap k a)
(\\) = difference

-- | Difference with a combining function.
--
-- @since 0.2.0.0@
--
differenceWith
    :: (Eq k, Hashable k, Elt a, Elt b)
    => (Exp a -> Exp b -> Exp (Maybe a))
    -> Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k a)
differenceWith f = differenceWithKey (const f)

-- | Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of the
-- keys. If it returns 'Nothing', the element is discarded (proper set
-- difference). If it returns (@'Just' y@), the element is updated with the
-- new value @y@.
--
-- @since 0.2.0.0@
--
differenceWithKey
    :: (Eq k, Hashable k, Elt a, Elt b)
    => (Exp k -> Exp a -> Exp b -> Exp (Maybe a))
    -> Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k a)
differenceWithKey f as bs = HashMap_ tree kv
  where
    kv    = afst
          $ justs
          $ A.map (\(T2 k va) -> lookup k bs & match \case
                                   Nothing_ -> Just_ (T2 k va)
                                   Just_ vb -> f k va vb & match \case
                                                 Just_ va' -> Just_ (T2 k va')
                                                 Nothing_  -> Nothing_) (assocs as)

    tree  = binary_radix_tree
          $ A.map (bitcast . hash . fst) kv


-- | Left-biased intersection of two maps. Returns the data in the first
-- map for the keys that exist in both.
--
-- @since 0.2.0.0@
--
intersection
    :: (Eq k, Hashable k, Elt a, Elt b)
    => Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k a)
intersection = intersectionWith const

-- | Intersection with a combining function
--
-- @since 0.2.0.0@
--
intersectionWith
    :: (Eq k, Hashable k, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k c)
intersectionWith f = intersectionWithKey (const f)

-- | Take the intersection of two maps with a combining function
--
-- @since 0.2.0.0@
--
intersectionWithKey
    :: (Eq k, Hashable k, Elt a, Elt b, Elt c)
    => (Exp k -> Exp a -> Exp b -> Exp c)
    -> Acc (HashMap k a)
    -> Acc (HashMap k b)
    -> Acc (HashMap k c)
intersectionWithKey f as bs = HashMap_ tree kv
  where
    -- Values from second map which are present in the first
    -- TODO: should we do the lookup into the smaller array or the larger?
    kv    = afst
          $ justs
          $ A.map (\(T2 k v2) -> lookup k as & match \case
                                   Nothing_ -> Nothing_
                                   Just_ v1 -> Just_ (T2 k (f k v1 v2))) (assocs bs)

    -- The (hashed) keys from the first step are still in sorted order,
    -- assuming 'justs' (a.k.a. 'filter') is stable, so no need to sort.
    tree  = binary_radix_tree
          $ A.map (bitcast . hash . fst) kv


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

-- | /O(1)/ The empty map
--
-- @since 0.2.0.0@
--
empty :: (Hashable k, Elt v) => Acc (HashMap k v)
empty = HashMap_ (fill (I1 0) undef) (fill (I1 0) undef)

-- | /O(1)/ A map with a single element
--
-- @since 0.2.0.0@
--
singleton :: (Hashable k, Elt v) => Exp k -> Exp v -> Acc (HashMap k v)
singleton k v = HashMap_ (fill (I1 0) undef) (fill (I1 1) (T2 k v))

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

