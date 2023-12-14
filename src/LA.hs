module LA
  ( Vector, each, pointwise, (.*), sumV
  , vFromList, vOneAt, vConcat
  , push, pop, empty
  , Matrix, mPointwise, mApp, mAppT, vToM
  , type Nat, type (+), KnownNat, natVal
  , Scalar
  , Zero(..)
  ) where

import GHC.TypeNats(Nat, type (+), KnownNat, natVal)
import Numeric(showFFloat)
import Control.Monad(guard)
import Data.Coerce(coerce)
import Data.Maybe(fromMaybe)
import Data.Word(Word64)
import Data.Proxy(Proxy(..))
import Data.Vector                 qualified as BV
import Data.Vector.Generic         qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as V
import Data.Binary qualified as B
import Text.PrettyPrint.HughesPJClass qualified as P
import Rng(Random(..))

class Zero a where
  zero :: a

--------------------------------------------------------------------------------
-- Scalars

newtype Scalar = Scalar Float
  deriving (Eq,Ord,Num,Fractional,Floating)

newtype instance V.MVector s Scalar = MVScalar (V.MVector s Float)
newtype instance V.Vector Scalar    = VScalar (V.Vector Float)
deriving instance VGM.MVector V.MVector Scalar
deriving instance VG.Vector   V.Vector  Scalar
instance V.Unbox Scalar

instance Zero Scalar where
  zero = 0

--------------------------------------------------------------------------------
-- Vector

newtype Vector (n :: Nat) = Vector { unV :: V.Vector Scalar }


empty :: Vector 0
empty = Vector V.empty
{-# INLINE empty #-}

push :: Scalar -> Vector n -> Vector (1 + n)
push x (Vector xs) = Vector (V.cons x xs)
{-# INLINE push #-}

pop :: Vector (1 + n) -> Vector n
pop (Vector xs) = Vector (V.tail xs)
{-# INLINE pop #-}

each :: (Scalar -> Scalar) -> Vector n -> Vector n
each f (Vector xs) = Vector (V.map f xs)
{-# INLINE each #-}

pointwise :: (Scalar -> Scalar -> Scalar) -> Vector n -> Vector n -> Vector n
pointwise f (Vector xs) (Vector ys) = Vector (V.zipWith f xs ys)
{-# INLINE pointwise #-}

(.*) :: Scalar -> Vector n -> Vector n
x .* xs = each (*x) xs
{-# INLINE (.*) #-}

sumV :: Vector n -> Scalar
sumV (Vector xs) = V.sum xs
{-# INLINE sumV #-}

instance KnownNat n => Zero (Vector n) where
  zero = Vector (V.replicate (knownInt (Proxy :: Proxy n)) 0)

--------------------------------------------------------------------------------
-- Matrix

-- | Non 0 sized matrices
newtype Matrix (m :: Nat) (n :: Nat) = Matrix { values :: V.Vector Scalar }

mPointwise ::
  (Scalar -> Scalar -> Scalar) -> Matrix m n -> Matrix m n -> Matrix m n
mPointwise f m n = m { values = V.zipWith f (values m) (values n) }
{-# INLINE mPointwise #-}

mApp :: Matrix m n -> Vector n -> Vector m
mApp m0 (Vector xs) = Vector (V.unfoldr step (values m0))
  where
  step m
    | V.null m = Nothing
    | otherwise =
      case V.splitAt (V.length xs) m of
        (this,next) -> Just (V.sum (V.zipWith (*) this xs), next)
{-# INLINE mApp #-}

mAppT :: Matrix m n -> Vector m -> Vector n
mAppT (Matrix m) (Vector v) = Vector (V.generate cols element)
  where
  rows      = V.length v
  cols      = V.length m `div` rows
  element c = sum [ m V.! (r * cols + c) * (v V.! r) | r <- take rows [ 0 .. ] ]



vToM :: Vector m -> (Scalar -> Vector n) -> Matrix m n
vToM (Vector xs) f = Matrix { values = V.concatMap (unV . f) xs }
{-# INLINE vToM #-}



mToList :: forall m n. KnownNat m => Matrix m n -> BV.Vector (Vector n)
mToList m0 = BV.unfoldr step (values m0)
  where
  cols = V.length (values m0) `div` knownInt (Proxy :: Proxy m)
  step m
    | V.null m  = Nothing
    | otherwise = case V.splitAt cols m of
                    (this,next) -> Just (Vector this, next)

vFromList :: forall n. KnownNat n => [Scalar] -> Vector n
vFromList xs = Vector (V.unfoldrExactN n step xs)
  where
  n = knownInt (Proxy :: Proxy n)
  step els =
    case els of
      []     -> (0,[])
      e : es -> (e, es)

-- | Zero everywhere, except 1 at the given index.
vOneAt :: forall n. KnownNat n => Int -> Vector n
vOneAt i = Vector (V.generate n \j -> if i == j then 1 else 0)
  where
  n = knownInt (Proxy :: Proxy n)

vConcat :: forall m n. KnownNat n => [Vector m] -> Vector n
vConcat xs =
  Vector
  case compare (V.length ys) n of
    EQ -> ys
    LT -> V.generate n \i -> fromMaybe 0 (ys V.!? i)
    GT -> V.take n ys
  where
  n  = knownInt (Proxy :: Proxy n)
  ys = V.concat (coerce xs)

knownInt :: KnownNat n => f n -> Int
knownInt = fromIntegral . natVal
{-# INLINE knownInt #-}

instance (KnownNat m, KnownNat n) => Zero (Matrix m n) where
  zero = Matrix (V.replicate s 0)
    where
    s = knownInt (Proxy :: Proxy m) * knownInt (Proxy :: Proxy n)

--------------------------------------------------------------------------------
-- Save & Load

instance B.Binary Scalar where
  put (Scalar n) = B.put n
  get            = Scalar <$> B.get

instance KnownNat n => B.Binary (Vector n) where
  put   = V.mapM_ B.put . unV
  get   = do let n = knownInt (Proxy :: Proxy n)
             guard (n >= 0)
             Vector <$> V.replicateM n B.get

instance (KnownNat m, KnownNat n) => B.Binary (Matrix m n) where
  put m = V.mapM_ B.put (values m)
  get   = do let r = knownInt (Proxy :: Proxy m)
                 c = knownInt (Proxy :: Proxy n)
                 s = r * c
             guard (r > 0 && c > 0)
             Matrix <$> V.replicateM s B.get

--------------------------------------------------------------------------------
-- Random

instance Random Scalar where
  random =
    do w <- random
       let scaled = fromIntegral (w :: Word64) /
                    fromIntegral (maxBound :: Word64)
       pure (Scalar (scaled - 0.5))

instance KnownNat n => Random (Vector n) where
  random =
    do let n = knownInt (Proxy :: Proxy n)
       Vector <$> V.replicateM n random

instance (KnownNat m, KnownNat n) => Random (Matrix m n) where
  random =
    do let r = knownInt (Proxy :: Proxy m)
           c = knownInt (Proxy :: Proxy n)
           s = r * c
       Matrix <$> V.replicateM s random


--------------------------------------------------------------------------------
-- Show & Pretty

instance Show Scalar where
  showsPrec _ (Scalar n) = showFFloat (Just 4) n

instance Show (Vector n) where
  showsPrec p (Vector xs) = showsPrec p xs

instance (KnownNat m) => Show (Matrix m n) where
  showsPrec p = showsPrec p . mToList


instance P.Pretty Scalar where
  pPrint = P.text . show

instance P.Pretty (Vector n) where
  pPrint = P.text . show

instance (KnownNat m) => P.Pretty (Matrix m n) where
  pPrint m = P.vcat (zipWith (P.<+>) start rows) P.$$ "]"
    where
    rows  = map P.pPrint (BV.toList (mToList m))
    start = "[" : repeat ","





