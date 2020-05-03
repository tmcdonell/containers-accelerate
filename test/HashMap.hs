{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HashMap where

import Gen

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Bits                              as A
import Data.Array.Accelerate.Data.Hashable                          as A
import Data.Array.Accelerate.Data.HashMap                           as HashMap

import Data.Typeable
import Data.Function
import Data.List                                                    as P
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
    testElt :: forall e. (P.Ord e, A.Hashable e, A.Eq e)
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: e)))
        [ testProperty "lookup"                $ test_lookup runN e int
        , testProperty "lookup-with-collision" $ test_lookup runN (collides e) int
        , testProperty "insert"                $ test_insert runN e int
        , testProperty "delete"                $ test_delete runN e int
        ]


_MAX_SIZE :: Int
_MAX_SIZE = 1024

test_lookup
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_lookup runN k v =
  property $ do
    m  <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    let n = Map.size m
    t1 <- if Map.null m
             then return []
             else forAll $ G.list (R.linear 0 n) (G.element (Map.keys m))
    t2 <-         forAll $ G.list (R.linear 0 n) k
    t  <- forAll $ G.shuffle (t1 P.++ t2)
    let !go = runN $ \hm -> A.map (`HashMap.lookup` HashMap.fromVector hm)
    --
    A.toList (go (fromMap m) (A.fromList (Z :. P.length t) t)) === P.map (`Map.lookup` m) t


test_insert
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_insert runN = test_insertWith runN const const

test_insertWith
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp v -> Exp v -> Exp v)
    -> (v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_insertWith runN f g = test_insertWithKey runN (const f) (const g)

test_insertWithKey
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp k -> Exp v -> Exp v -> Exp v)
    -> (k -> v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_insertWithKey runN f g k v =
  property $ do
    m <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    n <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    let !go = runN (\hm kv -> HashMap.assocs $ HashMap.insertWithKey f kv (HashMap.fromVector hm))
    --
    P.sortBy (P.compare `on` P.fst) (A.toList (go (fromMap m) (fromMap n))) ===
      Map.toAscList (P.foldr (\(k',v') m' -> Map.insertWithKey g k' v' m') m (Map.toList n))


test_delete
    :: (P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_delete runN k v =
  property $ do
    m <- forAll $ G.map  (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    n <- forAll $ G.list (R.linear 0 _MAX_SIZE) k
    let !go = runN (\hm ks -> HashMap.assocs $ HashMap.delete ks (HashMap.fromVector hm))
    --
    P.sortBy (P.compare `on` P.fst) (A.toList (go (fromMap m) (fromList (Z :. P.length n) n))) ===
      Map.toAscList (P.foldr Map.delete m n)


fromMap :: (Elt k, Elt v) => Map k v -> Vector (k,v)
fromMap m =
  let n = Map.size m
   in fromList (Z :. n) (Map.toList m)

collides :: Gen a -> Gen (Collides a)
collides g = Collides <$> g

newtype Collides a = Collides a
  deriving (Show, P.Eq, P.Ord, Generic, Elt, IsProduct Elt)

pattern Collides_ :: Elt a => Exp a -> Exp (Collides a)
pattern Collides_ x = Pattern x
{-# COMPLETE Collides_ #-}

instance Hashable a => Hashable (Collides a) where
  hash (Collides_ x) = 0xffff .&. hash x
  hashWithSalt       = defaultHashWithSalt

instance A.Eq a => A.Eq (Collides a) where
  Collides_ x == Collides_ y = x A.== y
  Collides_ x /= Collides_ y = x A./= y

