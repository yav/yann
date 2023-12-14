module Sample where

import LA

data Sample i o = Sample
  { sampleLabel   :: String   -- ^ Used for debugging
  , sampleInput   :: Vector i
  , sampleOutput  :: Vector o
  }

class NetValue a where
  type NetValueWidth a :: Nat
  valToVec   :: a -> Vector (NetValueWidth a)
  valFromVec :: Vector (NetValueWidth a) -> a

instance NetValue Bool where
  type NetValueWidth Bool = 1
  valToVec b = if b then vOneAt 0 else zero
  valFromVec v = vIndex v 0 >= 0.5