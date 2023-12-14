module Scalar where

{-
import Numeric
import Data.Word
import Data.Vector.Unboxed         qualified as V
import Data.Vector.Generic         qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Binary                 qualified as B
import Rng

newtype Scalar = Scalar Float
  deriving (Eq,Ord,Num,Fractional,Floating)

newtype instance V.MVector s Scalar = MVScalar (V.MVector s Float)
newtype instance V.Vector Scalar    = VScalar (V.Vector Float)
deriving instance VGM.MVector V.MVector Scalar
deriving instance VG.Vector   V.Vector  Scalar
instance V.Unbox Scalar

instance Show Scalar where
  showsPrec _ (Scalar n) = showFFloat (Just 6) n

instance B.Binary Scalar where
  put (Scalar n) = B.put n
  get            = Scalar <$> B.get

instance Random Scalar where
  random =
    do w <- random
       let scaled = fromIntegral (w :: Word64) /
                    fromIntegral (maxBound :: Word64)
       pure (Scalar (scaled - 0.5))
-}
