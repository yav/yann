{-# Language GADTs #-}
module CList where

import GHC.TypeLits
import Data.Type.Equality

data CList f (x :: Nat) (y :: Nat) where
  (:>) :: KnownNat b => f a b -> CList f b c -> CList f a c
  Id   :: CList f a a

mapCList ::
  (forall a b. f a b -> g a b) ->
  CList f x y -> CList g x y
mapCList f xs =
  case xs of
    Id -> Id
    x :> xs' -> f x :> mapCList f xs'

pointwiseCList ::
  (forall a b. f a b -> g a b -> h a b) ->
  CList f x y -> CList g x y -> CList h x y
pointwiseCList f xs ys =
  case (xs,ys) of
    (Id,Id) -> Id
    (x :> xs', y :> ys')
      | Just Refl <- sameNat x y -> f x y :> pointwiseCList f xs' ys'
    _ -> error "pointwiseCList: mismatched shapes"
         -- or we could do `Id`, but it's error prone
