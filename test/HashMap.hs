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
    testElt :: forall e. (Show e, Typeable e, P.Ord e, A.Hashable e, A.Eq e)
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: e)))
        [ testProperty "lookup"                $ test_lookup runN e int
        , testProperty "lookup-with-collision" $ test_lookup runN (collides e) int
        , testProperty "insert"                $ test_insert runN e int
        , testProperty "delete"                $ test_delete runN e int
        , testProperty "union"                 $ test_union runN e int
        , testProperty "difference"            $ test_difference runN e int
        , testProperty "intersection"          $ test_intersection runN e int
        ]


_MAX_SIZE :: Int
_MAX_SIZE = 1024

test_lookup
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
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
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_insert runN = test_insertWith runN const const

test_insertWith
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp v -> Exp v -> Exp v)
    -> (v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_insertWith runN f g = test_insertWithKey runN (const f) (const g)

test_insertWithKey
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
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
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
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


test_union
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_union runN = test_unionWith runN const const

test_unionWith
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp v -> Exp v -> Exp v)
    -> (v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_unionWith runN f g = test_unionWithKey runN (const f) (const g)

test_unionWithKey
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp k -> Exp v -> Exp v -> Exp v)
    -> (k -> v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_unionWithKey runN f g k v =
  property $ do
    m <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    n <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    let !go = runN (\kv1 kv2 -> HashMap.assocs $ HashMap.unionWithKey f (HashMap.fromVector kv1) (HashMap.fromVector kv2))
    --
    P.sortBy (P.compare `on` P.fst) (A.toList (go (fromMap m) (fromMap n))) ===
      Map.toAscList (Map.unionWithKey g m n)


test_difference
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_difference runN = test_differenceWith runN (\_ _ -> Nothing_) (\_ _ -> Nothing)

test_differenceWith
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp v -> Exp v -> Exp (Maybe v))
    -> (v -> v -> Maybe v)
    -> Gen k
    -> Gen v
    -> Property
test_differenceWith runN f g = test_differenceWithKey runN (const f) (const g)

test_differenceWithKey
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp k -> Exp v -> Exp v -> Exp (Maybe v))
    -> (k -> v -> v -> Maybe v)
    -> Gen k
    -> Gen v
    -> Property
test_differenceWithKey runN f g k v =
  property $ do
    m <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    n <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    let !go = runN (\kv1 kv2 -> HashMap.assocs $ HashMap.differenceWithKey f (HashMap.fromVector kv1) (HashMap.fromVector kv2))
    --
    P.sortBy (P.compare `on` P.fst) (A.toList (go (fromMap m) (fromMap n))) ===
      Map.toAscList (Map.differenceWithKey g m n)


test_intersection
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> Gen k
    -> Gen v
    -> Property
test_intersection runN = test_intersectionWith runN const const

test_intersectionWith
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp v -> Exp v -> Exp v)
    -> (v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_intersectionWith runN f g = test_intersectionWithKey runN (const f) (const g)

test_intersectionWithKey
    :: (Show k, Show v, P.Ord k, P.Eq v, A.Hashable k, A.Eq k, A.Eq v)
    => RunN
    -> (Exp k -> Exp v -> Exp v -> Exp v)
    -> (k -> v -> v -> v)
    -> Gen k
    -> Gen v
    -> Property
test_intersectionWithKey runN f g k v =
  property $ do
    m <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    n <- forAll $ G.map (R.linear 0 _MAX_SIZE) ((,) <$> k <*> v)
    let !go = runN (\kv1 kv2 -> HashMap.assocs $ HashMap.intersectionWithKey f (HashMap.fromVector kv1) (HashMap.fromVector kv2))
    --
    P.sortBy (P.compare `on` P.fst) (A.toList (go (fromMap m) (fromMap n))) ===
      Map.toAscList (Map.intersectionWithKey g m n)


fromMap :: (Elt k, Elt v) => Map k v -> Vector (k,v)
fromMap m =
  let n = Map.size m
   in fromList (Z :. n) (Map.toList m)

collides :: Gen a -> Gen (Collides a)
collides g = Collides <$> g

newtype Collides a = Collides a
  deriving (Show, P.Eq, P.Ord, Generic, Elt)

pattern Collides_ :: Elt a => Exp a -> Exp (Collides a)
pattern Collides_ x = Pattern x
{-# COMPLETE Collides_ #-}

instance Hashable a => Hashable (Collides a) where
  hash (Collides_ x) = 0xffff .&. hash x
  hashWithSalt       = defaultHashWithSalt

instance A.Eq a => A.Eq (Collides a) where
  Collides_ x == Collides_ y = x A.== y
  Collides_ x /= Collides_ y = x A./= y

