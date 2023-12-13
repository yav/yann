module Net
  ( Layers(..)
  , Net(..)
  , evalNet
  , netG
  , updateNet
  , Norm(..)
  ) where

import Data.Binary qualified as B
import Data.Kind(Type)
import Data.Proxy(Proxy(..))
import LA
import Act

type data Layers = Size Nat Norm Layers
                 | Final

data family Net :: Nat -> Layers -> Nat -> Type

data instance Net n Final n          = Result
data instance Net i (Size s n hs) o  = Layer i s :> Net s hs o

type Layer i o                       = Matrix o (1 + i)
type LayerG i o                      = Layer i o
type NetG                            = Net

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a layer.
evalLayer :: EvalNorm n => f n -> Layer i o -> Vector i -> Vector o
evalLayer n m is = evalNorm n (m `mApp` push 1 is)
{-# Inline evalLayer #-}

-- | Evaluate a network.
class EvalNet i h o where
  evalNet :: Net i h o -> Vector i -> Vector o

instance (i ~ o) => EvalNet i Final o where
  evalNet _ = id
  {-# Inline evalNet #-}

instance (EvalNorm n, EvalNet s hs o) => EvalNet i (Size s n hs) o where
  evalNet (m :> ms) = evalNet ms . evalLayer (Proxy :: Proxy n) m
  {-# Inline evalNet #-}

--------------------------------------------------------------------------------
-- Gradients

class EvalExample i h o where

  -- | How the loss function changes for the given example,
  -- if we chnage the weights of the net.
  netG  :: Net i h o -> Vector i -> Vector o -> NetG i h o

  -- | Two gardients:
  --    1. First one is as above (change wrt weights).
  --    2. How the loss changes if we change one of our
  --       *unnormalized* inputs inputs.
  nextNetG ::
    EvalNorm n =>
    f n  {- ^ How our inputs were normalized -} ->
    Vector i ->
    Vector o ->
    Net i h o ->
    (NetG i h o, Vector i)

instance (i ~ o) => EvalExample i Final o where
  netG _ _ _ = Result
  {-# Inline netG #-}

  nextNetG norm os expected _ = (Result, pointwise delta os expected)
    where delta = evalLossDeltaOutput norm
  {-# Inline nextNetG #-}

instance (EvalNorm n, EvalExample s h o) => EvalExample i (Size s n h) o where
  netG (l :> ls) ins expected = fst (layerG expected ins l ls)
  {-# Inline netG #-}

  nextNetG norm os expected (l :> next) = (nextG, updateIGradient norm l os igs)
    where
    (nextG, igs) = layerG expected os l next
  {-# Inline nextNetG #-}

layerG ::
  forall n i s h os.
  (EvalNorm n, EvalExample s h os) =>
  Vector os -> Vector i -> Layer i s -> Net s h os ->
  (NetG i (Size s n h) os, Vector s)
layerG expected is l next = (wGradients :> nextG, igs)
  where
  norm         = Proxy :: Proxy n
  os           = evalLayer norm l is
  (nextG, igs) = nextNetG norm os expected next
  wGradients   = igs `vToM` (.* push 1 is)
{-# Inline layerG #-}


updateIGradient ::
  EvalNorm norm =>
  f norm      {- ^ How our inputs were normalized -} ->
  Layer i o   {- ^ The layer we are going across  -} ->
  Vector i    {- ^ *Normalized* inputs to this layer -} ->
  Vector o    {- ^ How changes to our linear outputs affect the loss,
                  (i.e., the derivative of the normalization function
                  is included). -} ->
  Vector i    {- ^ How changes to our unnormalized inputs affect the loss
                   (i.e., the derivative of the previous normalization
                   function is included). -}
updateIGradient f l is ogs =
  pointwise (evalLossDeltaLayer f) is (pop (transpose l `mApp` ogs))
{-# Inline updateIGradient #-}

--------------------------------------------------------------------------------
-- Updating the weights

updateLayer :: Scalar -> Layer i o -> LayerG i o -> Layer i o
updateLayer eta = mPointwise \w g -> w - eta * g
{-# Inline updateLayer #-}

class UpdateNet i h o where
  updateNet' :: Scalar -> Net i h o -> NetG i h o -> Net i h o

instance (i ~ o) => UpdateNet i Final o where
  updateNet' _ h _ = h
  {-# Inline updateNet' #-}

instance UpdateNet s ls o => UpdateNet i (Size s n ls) o where
  updateNet' eta (m :> ms) (g :> gs) =
    updateLayer eta m g :> updateNet' eta ms gs
  {-# Inline updateNet' #-}

updateNet ::
  UpdateNet i h o => Scalar -> Int -> Net i h o -> NetG i h o -> Net i h o
updateNet eta samples n g
  | samples == 0 = n
  | otherwise    = updateNet' (eta / fromIntegral samples) n g

--------------------------------------------------------------------------------
-- Save & Load

instance (is ~ os) => B.Binary (Net is Final os) where
  put _ = pure ()
  get   = pure Result

instance (KnownNat (1+i), KnownNat s, B.Binary (Net s ls o)) =>
  B.Binary (Net i (Size s n ls) o) where
  put (l :> ls) = B.put l >> B.put ls
  get           = (:>) <$> B.get <*> B.get

