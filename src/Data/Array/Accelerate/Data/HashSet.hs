{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Data.Array.Accelerate.Data.HashSet
-- Copyright   : [2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.HashSet (

  HashSet, Hashable,

  -- * Construction
  fromVector,

  -- * Basic interface
  size,
  member,
  insert,
  delete,

  -- * Conversions
  elems,
  toMap,
  fromMap,

) where

import Data.Array.Accelerate                              hiding ( size )
import Data.Array.Accelerate.Data.HashMap                 ( HashMap, Hashable )
import qualified Data.Array.Accelerate                    as A
import qualified Data.Array.Accelerate.Data.HashMap       as M


-- | A set of values. A set can not contain duplicate values
--
data HashSet a = HashSet (HashMap a ())
  deriving (Show, Generic, Arrays)

pattern HashSet_
    :: Elt a
    => Acc (HashMap a ())
    -> Acc (HashSet a)
pattern HashSet_ hm = Pattern hm
{-# COMPLETE HashSet_ #-}

-- | /O(1)/ Return the number of key-value mappings
--
size :: Elt a => Acc (HashSet a) -> Exp Int
size = M.size . toMap

-- | /O(k)/ Return 'True' if the specified value is present in the set,
-- 'False' otherwise
--
member :: (Eq a, Hashable a) => Exp a -> Acc (HashSet a) -> Exp Bool
member k = M.member k . toMap

-- | Add the specified value to this set
--
insert :: (Eq a, Hashable a)
       => Acc (Vector a)
       -> Acc (HashSet a)
       -> Acc (HashSet a)
insert xs = fromMap . M.insert (A.map (\x -> T2 x (constant ())) xs) . toMap

-- | Remove the specified value from the set if present
--
delete :: (Eq a, Hashable a)
       => Acc (Vector a)
       -> Acc (HashSet a)
       -> Acc (HashSet a)
delete xs = fromMap . M.delete xs . toMap

-- | /O(1)/ Return the sets elements
--
elems :: Elt a => Acc (HashSet a) -> Acc (Vector a)
elems = M.keys . toMap

-- | /O(1)/ Convert to the equivalent 'HashMap'
--
toMap :: Elt a => Acc (HashSet a) -> Acc (HashMap a ())
toMap (HashSet_ m) = m

-- | /O(1)/ Convert from the equivalent 'HashMap'
--
fromMap :: Elt a => Acc (HashMap a ()) -> Acc (HashSet a)
fromMap = HashSet_

-- | /O(n log n)/ Construct the set from the specified values
--
fromVector :: Hashable a => Acc (Vector a) -> Acc (HashSet a)
fromVector = fromMap . M.fromVector . A.map (\x -> T2 x (constant ()))

