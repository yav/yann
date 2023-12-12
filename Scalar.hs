{-# LANGUAGE TypeFamilies #-}
module Scalar where

import Numeric

class Cmp a where
  type B a
  (==.) :: a -> a -> B a
  (<=.) :: a -> a -> B a
  (<.)  :: a -> a -> B a
  ite   :: B a -> a -> a -> a

class (Floating a, Cmp a) => IsScalar a

newtype Scalar = Scalar Float
  deriving (Eq,Ord,Num,Fractional,Floating)

instance Show Scalar where
  showsPrec _ (Scalar n) = showFFloat (Just 6) n

instance Cmp Scalar where
  type B Scalar = Bool
  (==.) = (==)
  (<=.) = (<=)
  (<.)  = (<)
  ite b t e = if b then t else e

instance IsScalar Scalar




