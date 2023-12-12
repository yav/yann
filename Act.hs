module Act where

import Scalar
import LA

sigmoid :: IsScalar s => s -> s
sigmoid x = recip (1 + exp (-x))

softmax :: IsScalar s => Vector n s -> Vector n s
softmax xs = (/ sum ys) <$> ys
  where
  ys = exp <$> xs

relu :: IsScalar s => s -> s
relu x = ite (0 <. x) x 0

squareErrorLoss :: IsScalar s => Vector n s -> Vector n s -> s
squareErrorLoss expected actual = sum (pointwise each expected actual) / 2
  where
  each e a = (e - a)^(2::Int)

crossEntropyLoss :: IsScalar s => Vector n s -> Vector n s -> s
crossEntropyLoss expected actual = -sum (pointwise each expected actual)
  where
  each e a = e * log a


