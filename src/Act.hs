module Act where

import LA

-- | Various normalization (aka. activation) functions.
type data Norm = Sigmoid | Softmax | ReLU

class NormFun (norm :: Norm) where
  showNorm            :: f norm -> String
  evalNorm            :: f norm -> Vector n -> Vector n
  evalLossDeltaLayer  :: f norm -> Scalar -> Scalar -> Scalar
  evalLossDeltaOutput :: f norm -> Scalar -> Scalar -> Scalar
  -- ^ Directional loss, combined with a paricular loss function

  evalError           :: f norm -> Vector n -> Vector n -> Scalar


instance NormFun ReLU where
  showNorm _ = "ReLU"
  evalNorm _ = each (max 0)
  evalLossDeltaLayer _ err o  = if o < 0 then 0 else err
  evalLossDeltaOutput _ e a = if e < a then 1 else 0 -- Square Error Loss
  evalError _ = squareErrorLoss


instance NormFun Sigmoid where
  showNorm _ = "Sigmoid"
  evalNorm _ = each \x -> recip (1 + exp (-x))
  evalLossDeltaLayer _ err o  = err * o * (1 - o)
  evalLossDeltaOutput _ a e = (a - e) * a * (1 - a) -- Square Error Loss
  evalError _ = squareErrorLoss

instance NormFun Softmax where
  showNorm _ = "Softmax"
  evalNorm _ xs = each (/ sumV ys) ys
    where ys = each exp xs
  evalLossDeltaLayer _      = error "Softmax in hidden layer not implemented"
  evalLossDeltaOutput _ a e = a - e                  -- Cross Entropy Loss,
  evalError _ = crossEntropyLoss



squareErrorLoss :: Vector n -> Vector n -> Scalar
squareErrorLoss expected actual = sumV (pointwise err expected actual) / 2
  where
  err e a = (e - a)^(2::Int)

crossEntropyLoss :: Vector n -> Vector n -> Scalar
crossEntropyLoss expected actual = -sumV (pointwise err expected actual)
  where
  err e a = e * log a


