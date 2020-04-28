{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Gen where

import Data.Array.Accelerate                                        ( Acc, Arrays, Array, Shape, Elt, DIM0, DIM1, DIM2, DIM3, Z(..), (:.)(..), fromList )
import Data.Array.Accelerate.Trafo                                  ( Afunction, AfunctionR )
import Data.Array.Accelerate.Array.Sugar                            ( size )

import Data.Int
import Data.Word
import Control.Monad
import Numeric.Half
import Hedgehog
import qualified Hedgehog.Gen                                       as G
import qualified Hedgehog.Range                                     as R


type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

dim0 :: Gen DIM0
dim0 = return Z

dim1 :: Gen DIM1
dim1 = (Z :.) <$> G.int (R.linear 0 1024)

dim2 :: Gen DIM2
dim2 = do
  x <- G.int (R.linear 0 128)
  y <- G.int (R.linear 0 48)
  return (Z :. y :. x)

dim3 :: Gen DIM3
dim3 = do
  x <- G.int (R.linear 0 64)
  y <- G.int (R.linear 0 32)
  z <- G.int (R.linear 0 16)
  return (Z :. z :. y :. x)

array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen = fromList sh <$> G.list (R.singleton (size sh)) gen

int :: Gen Int
int = G.int R.linearBounded

i8 :: Gen Int8
i8 = G.int8 R.linearBounded

i16 :: Gen Int16
i16 = G.int16 R.linearBounded

i32 :: Gen Int32
i32 = G.int32 R.linearBounded

i64 :: Gen Int64
i64 = G.int64 R.linearBounded

word :: Gen Word
word = G.word R.linearBounded

w8 :: Gen Word8
w8 = G.word8 R.linearBounded

w16 :: Gen Word16
w16 = G.word16 R.linearBounded

w32 :: Gen Word32
w32 = G.word32 R.linearBounded

w64 :: Gen Word64
w64 = G.word64 R.linearBounded

f16 :: Gen Half
f16 = G.realFloat (R.linearFracFrom 0 (-log_flt_max) log_flt_max)

f32 :: Gen Float
f32 = G.float (R.linearFracFrom 0 (-log_flt_max) log_flt_max)

f64 :: Gen Double
f64 = G.double (R.linearFracFrom 0 (-log_flt_max) log_flt_max)

-- v2 :: Prim a => Gen a -> Gen (V2 a)
-- v2 a = V2 <$> a <*> a

-- v3 :: Prim a => Gen a -> Gen (V3 a)
-- v3 a = V3 <$> a <*> a <*> a

-- v4 :: Prim a => Gen a -> Gen (V4 a)
-- v4 a = V4 <$> a <*> a <*> a <*> a

-- v8 :: Prim a => Gen a -> Gen (V8 a)
-- v8 a = V8 <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a

-- v16 :: Prim a => Gen a -> Gen (V16 a)
-- v16 a = V16 <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a
--             <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a

log_flt_max :: RealFloat a => a
log_flt_max = log flt_max

flt_max :: RealFloat a => a
flt_max = x
  where
    n     = floatDigits x
    b     = floatRadix x
    (_,u) = floatRange x
    x     = encodeFloat (b^n - 1) (u - n)

flt_min :: RealFloat a => a
flt_min = x
  where
    n     = floatDigits x
    b     = floatRadix x
    (l,_) = floatRange x
    x     = encodeFloat (b^n - 1) (l - n - 1)

except :: Gen e -> (e -> Bool) -> Gen e
except gen f  = do
  v <- gen
  when (f v) G.discard
  return v

