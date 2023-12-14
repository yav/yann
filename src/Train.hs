module Train(simpleTrain, Samples) where

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Control.Parallel
import Control.Monad(foldM)
import LA
import Net
import Rng

import Debug.Trace

type Samples i o = V.Vector (Vector i, Vector o)

simpleTrain ::
  IsNet i l o =>
  Samples i o {- ^ Training data -} ->
  Int         {- ^ Mini batch size -} ->
  Int         {- ^ Epochs -} ->
  Net i l o   {- ^ Net -} ->
  RngM (Net i l o)
simpleTrain samples batch n net = foldM (doEpoch samples batch) net rates
  where
  rates = take n (zip [1..] (iterate (* 0.9999) 0.1))
  -- There are better methods to adapt the learning rate parameter:
  -- Adagrad: use separate learning rates for each dimension.
  -- Adadelta: https://arxiv.org/pdf/1212.5701v1.pdf

doEpoch ::
  IsNet i l o =>
  Samples i o          {- ^ Traing data -} ->
  Int                  {- ^ Batch size, > 0 -} ->
  Net i l o            {- ^ The net being trained -} ->
  (Int,Scalar)            {- ^ Learning Rate -} ->
  RngM (Net i l o)     {- ^ Updated net -}
doEpoch samples batch net0 (name,eta) =
  do traceM ("Epoch " ++ show name)
     order <- randPerm (V.length samples)
     case U.foldl' step (net0, zero, batch) order of
       (net1, grad, todo) -> pure (updateNet eta (batch - todo) net1 grad)

  where
  step (!net,grad,!todo) i
    | todo > 0  = (net, doOne net samples grad i, todo - 1)
    | otherwise =
      let net1 = updateNet eta batch net grad
      in step (net1, zero, batch) i



doOne ::
  IsNet i l o => Net i l o -> Samples i o -> NetG i l o -> Int -> NetG i l o
doOne net samples grad i =
  case samples V.! i of
    (inp,out) -> let g  = netG net inp out
                 in g `par` addG grad g
{-# Inline doOne #-}
