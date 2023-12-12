{-# Language DataKinds, ExplicitNamespaces #-}
module LA
  ( Vector, pointwise, (.*)
  , indexed
  , push, pop, empty
  , Matrix, transpose, mMap, mPointwise
  , type (+)
  , module Scalar
  ) where

import qualified Data.List as List
import Data.Coerce(coerce)
import GHC.TypeNats(Nat, type (+))
import Scalar

newtype Vector (n :: Nat) a = Vector [a]
  deriving (Functor,Foldable,Traversable)

instance Show a => Show (Vector n a) where
  showsPrec p (Vector xs) = showsPrec p xs

type Matrix m n a = Vector m (Vector n a)

empty :: Vector 0 a
empty = Vector []

push :: a -> Vector n a -> Vector (1 + n) a
push x (Vector xs) = Vector (x : xs)

pop :: Vector (1 + n) a -> Vector n a
pop (Vector xs) = Vector (tail xs)

(.*) :: IsScalar s => s -> Vector n s -> Vector n s
x .* xs = (*x) <$> xs

pointwise :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
pointwise f (Vector xs) (Vector ys) = Vector (zipWith f xs ys)

mMap :: (a -> b) -> Matrix m n a -> Matrix m n b
mMap f = fmap (fmap f)

mPointwise :: (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
mPointwise f = pointwise (pointwise f)

transpose :: Matrix m n a -> Matrix n m a
transpose = mk . List.transpose . coerce
  where
  mk :: [[a]] -> Matrix m n a
  mk = coerce

indexed :: Vector n a -> Vector n (Int,a)
indexed (Vector xs) = Vector (zip [0..] xs)





