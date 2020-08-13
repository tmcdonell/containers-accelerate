{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Sort.Quick where

import Gen

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Sort.Quick                        as A

import Data.Typeable
import Data.Function
import Data.List                                                    as P
import Prelude                                                      as P

import Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog


test_quicksort :: RunN -> TestTree
test_quicksort runN =
  testGroup "quicksort"
    [ testElt int
    -- , testElt i8
    -- , testElt i16
    -- , testElt i32
    -- , testElt i64
    , testElt word
    -- , testElt w8
    -- , testElt w16
    -- , testElt w32
    -- , testElt w64
    , testElt f32
    , testElt f64
    ]
  where
    testElt :: forall e. (Show e, Typeable e, P.Ord e, A.Ord e)
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: e)))
        [ testProperty "ascending"  $ test_sort_ascending runN e
        , testProperty "descending" $ test_sort_descending runN e
        , testProperty "key-value"  $ test_sort_keyval runN e f64
        ]

test_sort_ascending
    :: (Show e, P.Ord e, A.Ord e)
    => RunN
    -> Gen e
    -> Property
test_sort_ascending runN e =
  property $ do
    sh <- forAll dim1
    xs <- forAll (array sh e)
    let !go = runN A.sort in go xs === sortRef P.compare xs

test_sort_descending
    :: (Show e, P.Ord e, A.Ord e)
    => RunN
    -> Gen e
    -> Property
test_sort_descending runN e =
  property $ do
    sh <- forAll dim1
    xs <- forAll (array sh e)
    let !go = runN (A.sortBy (flip A.compare)) in go xs === sortRef (flip P.compare) xs

test_sort_keyval
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Ord k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_sort_keyval runN k v =
  property $ do
    sh <- forAll dim1
    xs <- forAll (array sh ((,) <$> k <*> v))
    let !go = runN (A.sortBy (A.compare `on` A.fst)) in go xs === sortRef (P.compare `on` P.fst) xs

sortRef :: Elt a => (a -> a -> Ordering) -> Vector a -> Vector a
sortRef cmp xs = fromList (arrayShape xs) (P.sortBy cmp (toList xs))

