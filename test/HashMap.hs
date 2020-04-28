{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HashMap where

import Gen

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Hashable                          as A
import Data.Array.Accelerate.Data.HashMap                           as HashMap

import Data.Typeable
import Prelude                                                      as P
import Data.Map.Strict                                              ( Map )
import qualified Data.Map.Strict                                    as Map

import Hedgehog
import qualified Hedgehog.Gen                                       as G
import qualified Hedgehog.Range                                     as R

import Test.Tasty
import Test.Tasty.Hedgehog


test_hashmap :: RunN -> TestTree
test_hashmap runN =
  testGroup "hashmap"
    [ testElt int
    ]
  where
    testElt :: forall e. (P.Ord e, A.Hashable e, A.Eq e)
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: e)))
        [ testProperty "lookup" $ test_lookup runN e f32
        ]


_MAX_SIZE :: Int
_MAX_SIZE = 2 P.^ (14::Int)

test_lookup
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_lookup runN k v =
  property $ do
    m  <- forAll $ G.map  (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    t1 <- if Map.null m
             then return []
             else forAll $ G.list (R.linear 0 _MAX_SIZE) (G.element (Map.keys m))
    t2 <-         forAll $ G.list (R.linear 0 _MAX_SIZE) k
    t  <- forAll $ G.shuffle (t1 P.++ t2)
    let !go = runN $ \hm -> A.map (`HashMap.lookup` HashMap.fromVector hm)
    --
    A.toList (go (fromMap m) (A.fromList (Z :. P.length t) t)) === P.map (`Map.lookup` m) t

fromMap :: (Elt k, Elt v) => Map k v -> Vector (k,v)
fromMap m =
  let n = Map.size m
   in fromList (Z :. n) (Map.toList m)

