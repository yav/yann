{-# LANGUAGE TemplateHaskellQuotes, BlockArguments, TypeFamilies #-}
module Compile where

import Language.Haskell.TH
import Control.Monad
import Scalar
import LA

newtype M r a = M { runM :: (a -> Code Q r) -> Code Q r }

instance Functor (M r) where
  fmap = liftM

instance Applicative (M r) where
  pure a = M (\k -> k a)
  (<*>)  = ap

instance Monad (M r) where
  M m >>= f = M \k -> m \a -> let M m2 = f a in m2 k

newtype CScalar = CScalar (Code Q Scalar)

cacheScalar :: CScalar -> M r CScalar
cacheScalar (CScalar s) =
  M \k -> [|| let x = $$s :: Scalar in $$(k (CScalar [|| x ||])) ||]

cacheVector :: Vector n CScalar -> M r (Vector n CScalar)
cacheVector = traverse cacheScalar

instance Cmp CScalar where
  type B CScalar = Code Q Bool
  CScalar x ==. CScalar y   = [|| $$x == ($$y :: Scalar) ||]
  CScalar x <=. CScalar y   = [|| $$x <= ($$y :: Scalar) ||]
  CScalar x <. CScalar y    = [|| $$x < ($$y :: Scalar) ||]
  ite b (CScalar x) (CScalar y) =
    CScalar [|| if $$b then $$x else $$y :: Scalar ||]

instance Num CScalar where
  CScalar x + CScalar y = CScalar [|| $$x + $$y       :: Scalar ||]
  CScalar x - CScalar y = CScalar [|| $$x - $$y       :: Scalar ||]
  CScalar x * CScalar y = CScalar [|| $$x * $$y       :: Scalar ||]
  negate (CScalar x)    = CScalar [|| negate $$x      :: Scalar ||]
  fromInteger x         = CScalar [|| fromInteger x   :: Scalar ||]
  abs (CScalar x)       = CScalar [|| abs $$x         :: Scalar ||]
  signum (CScalar x)    = CScalar [|| signum $$x      :: Scalar ||]

instance Fractional CScalar where
  CScalar x / CScalar y = CScalar [|| $$x / $$y       :: Scalar ||]
  recip (CScalar x)     = CScalar [|| recip $$x       :: Scalar ||]
  fromRational x        = CScalar [|| fromRational x  :: Scalar ||]

instance Floating CScalar where
  pi                    = CScalar [|| pi              :: Scalar ||]
  exp (CScalar x)       = CScalar [|| exp $$x         :: Scalar ||]
  log (CScalar x)       = CScalar [|| log $$x         :: Scalar ||]
  sqrt (CScalar x)      = CScalar [|| sqrt $$x        :: Scalar ||]
  CScalar x ** CScalar y= CScalar [|| $$x ** $$y      :: Scalar ||]
  logBase (CScalar x) (CScalar y) = CScalar [|| logBase $$x $$y :: Scalar ||]
  sin (CScalar x)       = CScalar [|| sin $$x         :: Scalar ||]
  cos (CScalar x)       = CScalar [|| cos $$x         :: Scalar ||]
  tan (CScalar x)       = CScalar [|| tan $$x         :: Scalar ||]
  asin (CScalar x)      = CScalar [|| asin $$x        :: Scalar ||]
  acos (CScalar x)      = CScalar [|| acos $$x        :: Scalar ||]
  atan (CScalar x)      = CScalar [|| atan $$x        :: Scalar ||]
  sinh (CScalar x)      = CScalar [|| sinh $$x        :: Scalar ||]
  cosh (CScalar x)      = CScalar [|| cosh $$x        :: Scalar ||]
  tanh (CScalar x)      = CScalar [|| tanh $$x        :: Scalar ||]
  asinh (CScalar x)     = CScalar [|| asinh $$x        :: Scalar ||]
  acosh (CScalar x)     = CScalar [|| acosh $$x        :: Scalar ||]
  atanh (CScalar x)     = CScalar [|| atanh $$x        :: Scalar ||]




