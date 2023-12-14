module Net
  ( Layers(..)
  , Net(..), NetG
  , evalNet
  , netG
  , addG
  , Zero(..)
  , updateNet
  , Norm(..)
  , IsNet
  ) where

import Data.Binary qualified as B
import Data.Kind(Type)
import Data.Proxy(Proxy(..))
import Text.PrettyPrint.HughesPJClass qualified as P
import LA
import Act
import Rng

type data Layers = Size Nat Norm Layers
                 | Final

data family Net :: Nat -> Layers -> Nat -> Type

data instance Net n Final n          = Result
data instance Net i (Size s n hs) o  = !(Layer i s) :> !(Net s hs o)

type Layer i o                       = Matrix o (1 + i)
type LayerG i o                      = Layer i o
type NetG                            = Net

type IsNet i l o = ( Layerwise i l o, EvalNet i l o, EvalSample i l o
                   , Zero (Net i l o)
                   , B.Binary (Net i l o)
                   , Random (Net i l o)
                   )
--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a layer.
evalLayer :: NormFun n => f n -> Layer i o -> Vector i -> Vector o
evalLayer n m is = evalNorm n (m `mApp` push 1 is)
{-# Inline evalLayer #-}

-- | Evaluate a network.
class EvalNet i h o where
  evalNet :: Net i h o -> Vector i -> Vector o

instance (i ~ o) => EvalNet i Final o where
  evalNet _ = id
  {-# Inline evalNet #-}

instance (NormFun n, EvalNet s hs o) => EvalNet i (Size s n hs) o where
  evalNet (m :> ms) = evalNet ms . evalLayer (Proxy :: Proxy n) m
  {-# Inline evalNet #-}

--------------------------------------------------------------------------------
-- Gradients

class EvalSample i h o where

  -- | How the loss function changes for the given example,
  -- if we chnage the weights of the net.
  netG  :: Net i h o -> Vector i -> Vector o -> NetG i h o

  -- | Two gardients:
  --    1. First one is as above (change wrt weights).
  --    2. How the loss changes if we change one of our
  --       *unnormalized* inputs inputs.
  nextNetG ::
    NormFun n =>
    f n  {- ^ How our inputs were normalized -} ->
    Vector i ->
    Vector o ->
    Net i h o ->
    (NetG i h o, Vector i)

instance (i ~ o) => EvalSample i Final o where
  netG _ _ _ = Result
  {-# Inline netG #-}

  nextNetG norm os expected _ = (Result, pointwise delta os expected)
    where delta = evalLossDeltaOutput norm
  {-# Inline nextNetG #-}

instance (NormFun n, EvalSample s h o) => EvalSample i (Size s n h) o where
  netG (l :> ls) ins expected = fst (layerG expected ins l ls)
  {-# Inline netG #-}

  nextNetG norm os expected (l :> next) = (nextG, updateIGradient norm l os igs)
    where
    (nextG, igs) = layerG expected os l next
  {-# Inline nextNetG #-}

layerG ::
  forall n i s h os.
  (NormFun n, EvalSample s h os) =>
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
  NormFun norm =>
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
  pointwise (evalLossDeltaLayer f) is (pop (l `mAppT` ogs))
{-# Inline updateIGradient #-}

--------------------------------------------------------------------------------
-- Updating the weights

class Layerwise i h o where
  layerwise ::
    (forall x y . Layer x y-> Layer x y-> Layer x y) ->
    Net i h o -> Net i h o -> Net i h o

instance (i ~ o) => Layerwise i Final o where
  layerwise _ _ _ = Result

instance Layerwise s ls o => Layerwise i (Size s n ls) o where
  layerwise f (l :> ls) (r :> rs) = f l r :> layerwise f ls rs

-- | Update a network based on the given cumulative gradient.
updateNet ::
  Layerwise i h o =>
  Scalar        {- ^ Learning rate -} ->
  Int           {- ^ Number of samples in the gradient -} ->
  Net i h o     {- ^ Original weights -} ->
  NetG i h o    {- ^ Cumulative gradient -} ->
  Net i h o     {- ^ Updated netwoek -}
updateNet eta samples n g
  | samples == 0 = n
  | otherwise    = layerwise (updateLayer (eta / fromIntegral samples)) n g

updateLayer :: Scalar -> Layer i o -> LayerG i o -> Layer i o
updateLayer eta = mPointwise \w g -> w - eta * g
{-# Inline updateLayer #-}

instance (i ~ o) => Zero (Net i Final o) where
  zero = Result

instance (Zero (Net s ls o), KnownNat (1+i), KnownNat s)  =>
  Zero (Net i (Size s n ls) o) where
  zero = zero :> zero

-- | Add two gradients.
addG :: Layerwise i l o => Net i l o -> NetG i l o -> NetG i l o
addG = layerwise (mPointwise (+))


--------------------------------------------------------------------------------
-- Save & Load

instance (is ~ os) => B.Binary (Net is Final os) where
  put _ = pure ()
  get   = pure Result

instance (KnownNat (1+i), KnownNat s, B.Binary (Net s ls o)) =>
  B.Binary (Net i (Size s n ls) o) where
  put (l :> ls) = B.put l >> B.put ls
  get           = (:>) <$> B.get <*> B.get

--------------------------------------------------------------------------------
-- Random

instance (is ~ os) => Random (Net is Final os) where
  random = pure Result

instance (KnownNat (1+i), KnownNat s, Random (Net s ls o)) =>
  Random (Net i (Size s n ls) o) where
  random = (:>) <$> random <*> random


--------------------------------------------------------------------------------
-- Show & Pretty

instance (i ~ o) => P.Pretty (Net i Final o) where
  pPrint _ = P.text (replicate 80 '=')

instance (KnownNat s, P.Pretty (Net s ls o)) =>
  P.Pretty (Net i (Size s n ls) o) where
  pPrint (l :> ls) = P.pPrint l P.$$ P.text (replicate 80 '-') P.$$ P.pPrint ls





