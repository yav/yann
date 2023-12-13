module LA
  ( Vector, each, pointwise, (.*), sumV
  , push, pop, empty
  , Matrix, transpose, mPointwise, mApp, vToM, mRows, mCols
  , type Nat, type (+), KnownNat, natVal
  , module Scalar
  ) where

import GHC.TypeNats(Nat, type (+), KnownNat, natVal)
import Control.Monad(guard)
import Data.Proxy(Proxy(..))
import Data.Vector qualified as GV
import Data.Vector.Unboxed qualified as V
import Data.Binary qualified as B
import Scalar


--------------------------------------------------------------------------------
-- Matrix

newtype Vector (n :: Nat) = Vector { unV :: V.Vector Scalar }

instance Show (Vector n) where
  showsPrec p (Vector xs) = showsPrec p xs

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



--------------------------------------------------------------------------------
-- Matrix

-- | Non 0 sized matrices
data Matrix (m :: Nat) (n :: Nat) =
  Matrix { rows :: Int, values :: V.Vector Scalar }

instance Show (Matrix m n) where
  showsPrec p = showsPrec p . mToList

mRows :: Matrix m n -> Int
mRows = rows
{-# INLINE mRows #-}

mCols :: Matrix m n -> Int
mCols m = V.length (values m) `div` mRows m
{-# INLINE mCols #-}

mPointwise ::
  (Scalar -> Scalar -> Scalar) -> Matrix m n -> Matrix m n -> Matrix m n
mPointwise f m n = m { values = V.zipWith f (values m) (values n) }
{-# INLINE mPointwise #-}

transpose :: Matrix m n -> Matrix n m
transpose m =
  Matrix { rows   = cols
         , values = V.generate n value
         }
  where
  n       = V.length xs
  cols    = mCols m
  xs      = values m
  value i = let (r,c) = divMod i (mRows m)
            in xs V.! (c * cols + r)
{-# INLINE transpose #-}

mApp :: Matrix m n -> Vector n -> Vector m
mApp m0 (Vector xs) = Vector (V.unfoldr step (values m0))
  where
  step m
    | V.null m = Nothing
    | otherwise =
      case V.splitAt (V.length xs) m of
        (this,next) -> Just (V.sum (V.zipWith (*) this xs), next)
{-# INLINE mApp #-}

vToM :: Vector m -> (Scalar -> Vector n) -> Matrix m n
vToM (Vector xs) f =
  Matrix
    { rows   = V.length xs
    , values = V.concatMap (unV . f) xs
    }
{-# INLINE vToM #-}

mToList :: Matrix m n -> GV.Vector (Vector n)
mToList m0 = GV.unfoldr step (values m0)
  where
  step m
    | V.null m  = Nothing
    | otherwise = case V.splitAt (mCols m0) m of
                    (this,next) -> Just (Vector this, next)

instance (KnownNat m, KnownNat n) => B.Binary (Matrix m n) where
  put m = V.mapM_ B.put (values m)
  get   = do let r = natVal (Proxy :: Proxy m)
                 c = natVal (Proxy :: Proxy n)
                 s = fromIntegral (r * c)
             guard (r > 0 && c > 0)
             vs <- V.replicateM s B.get
             pure Matrix { rows = fromIntegral r, values = vs }

