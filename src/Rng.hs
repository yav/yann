module Rng
  ( Random(..), RngM, runRngM, runBigSeedRngM, runSmallSeedRngM
  , randPerm, shuffle
  )
  where

import Control.Monad(liftM,ap)
import Control.Monad.ST.Strict
import Data.Word
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import System.Random.TF
import System.Random.TF.Instances qualified as R

newtype RngM a = RngM { withGen :: TFGen -> (a,TFGen) }

runRngM :: RngM a -> IO a
runRngM (RngM m) = fst . m <$> newTFGen

runBigSeedRngM :: (Word64,Word64,Word64,Word64) -> RngM a -> a
runBigSeedRngM seed (RngM m) = fst (m (seedTFGen seed))

runSmallSeedRngM :: Int -> RngM a -> a
runSmallSeedRngM seed (RngM m) = fst (m (mkTFGen seed))

instance Functor RngM where
  fmap = liftM

instance Applicative RngM where
  pure a = RngM \s -> (a,s)
  (<*>)  = ap

instance Monad RngM where
  RngM m >>= f = RngM \s -> case m s of
                              (a, g1) -> withGen (f a) $! g1

class Random a where
  random :: RngM a

instance Random Word64 where
  random = RngM R.random

shuffle :: BV.Vector a -> RngM (BV.Vector a)
shuffle xs =
  do let n = BV.length xs
     perm <- randPerm n
     pure (BV.generate n \i -> xs BV.! (perm V.! i))

randPerm :: Int -> RngM (V.Vector Int)
randPerm n =
  RngM \g0 ->
    runST
    do v  <- MV.generate n id
       g1 <- go v g0 0
       v1 <- V.freeze v
       pure (v1,g1)
  where
  term = n - 1

  go :: MV.MVector s Int -> TFGen -> Int -> ST s TFGen
  go v g i
    | i < term = case R.randomR (i,term) g of
                   (j,g1) -> MV.swap v i j >> go v g1 (i+1)
    | otherwise = pure g



