module Net where

import Data.Binary qualified as B
import Data.Kind(Type)
import Data.Proxy(Proxy(..))
import LA
import Act

type data NetKind =
  NetType
    Nat             {- ^ Number of inputs -}
    [LayerKind]     {- ^ Layer shapes -}
    Nat             {- ^ Number of outputs -}

-- | How many results, and how to normalize them.
type data LayerKind = Nat :~> Norm

type family NetInputs a where
  NetInputs (NetType is _ _) = is

type family NetOutputs a where
  NetOutputs (NetType _ _ os) = os

type family LayerOutputs a where
  LayerOutputs (n :~> _) = n

type family LayerNorm a where
  LayerNorm (_ :~> n) = n




-- | General topology of a neural net.
data NetTopology ::
      (Nat -> LayerKind -> Type) ->
      NetKind ->
      Type
  where
  Layer   :: (EvalNorm (LayerNorm l)) =>
             layer inputs l ->
             NetTopology layer (NetType (LayerOutputs l) hidden outputs) ->
             NetTopology layer (NetType inputs (l ': hidden) outputs)
  Result  :: NetTopology layer (NetType n '[] n)


-- | A layer of the network.
data Layer' ins k =
  Layer'
  { neurons :: Matrix (LayerOutputs k) (1 + ins) -- ^ Weights for the layer
  }

norm :: Layer' ins k -> Proxy (LayerNorm k)
norm _ = Proxy

-- | Gradient of a layer.
newtype GLayer ins k = GLayer
  { neuronGs :: Matrix (LayerOutputs k) (1 + ins) -- ^ How loss changes with inputs
  }


type Net  = NetTopology Layer'
type NetG = NetTopology GLayer

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a layer.
evalLayer ::
  EvalNorm (LayerNorm k) => Layer' ins k -> Vector ins -> Vector (LayerOutputs k)
evalLayer l is = evalNorm (norm l) (neurons l `mApp` push 1 is)

-- | Evaluate a network.
evalNet :: Net k -> Vector (NetInputs k) -> Vector (NetOutputs k)
evalNet net =
  case net of
    Result    -> id
    Layer l next -> evalNet next . evalLayer l

--------------------------------------------------------------------------------
-- Gradients

-- | Compute how the loss will change for a given input-output pair,
-- if we change any of the weights.
netG :: forall k os.
  (os ~ NetOutputs k) =>
  Net k -> Vector (NetInputs k) -> Vector os -> NetG k
netG net0 is0 expected =
  case net0 of
    Result       -> Result
    Layer l more -> fst (layerG is0 l more)

  where
  layerG ::
    (EvalNorm (LayerNorm lk), b ~ LayerOutputs lk) =>
    Vector a -> Layer' a lk -> Net (NetType b h os) ->
                                (NetG (NetType a (lk : h) os), Vector b)
  layerG is l next = (Layer wGradients newNext, igs)
    where
    os             = evalLayer l is
    wGradients     = GLayer (igs `vToM` (.* push 1 is))

    (newNext, igs) =
      case next of
        Result -> (Result, pointwise delta os expected)
          where delta = evalLossDeltaOutput (norm l)

        Layer l' next' -> (newNet, updateIGradient l' (norm l) os igs')
          where
          (newNet, igs') = layerG os l' next'


updateIGradient ::
  EvalNorm norm =>
  Layer' ins k -> -- outs ourNorm                                      ->
  f norm            {- ^ How our inputs were normalized -}    ->
  Vector ins        {- ^ *Normalized* inputs to this layer -} ->
  Vector (LayerOutputs k) {- ^ How changes to our linear outputs affect the loss,
                         (i.e., the derivative of the normalization function
                         is included). -}                      ->
  Vector ins        {- ^ How changes to our unnormalized inputs affect the loss
                         (i.e., the derivative of the previous normalization
                          function is included). -}
updateIGradient l f is ogs = pointwise (evalLossDeltaLayer f) is gradients
  where
  -- The `pop` is to skip the weight of the bias input
  gradients = pop (transpose (neurons l) `mApp` ogs)

  --------------------------------------------------------------------------------
  -- Updating the weights

updateLayer :: Scalar -> Layer' ins k -> GLayer ins k -> Layer' ins k
updateLayer eta l lg =
  l { neurons = mPointwise updateWeight (neurons l) (neuronGs lg) }
  where
  updateWeight w g = w - eta * g

updateNet :: Scalar -> Net k -> Scalar -> NetG k -> Net k
updateNet eta net0 samples g0
  | samples == 0 = net0
  | otherwise    = go net0 g0
  where
  eta' = eta / samples

  go :: Net k -> NetG k -> Net k
  go net g =
    case (net,g) of
      (Result,Result)          -> Result
      (Layer l more, Layer lg moreG) ->
        Layer (updateLayer eta' l lg) (go more moreG)

--------------------------------------------------------------------------------

instance (KnownNat (1+is), KnownNat (LayerOutputs k)) => B.Binary (Layer' is k) where
  put l = B.put (neurons l)
  get =
    do ns <- B.get
       pure Layer' { neurons = ns }

instance (is ~ os) => B.Binary (Net (NetType is '[] os)) where
  put _ = pure ()
  get   = pure Result

instance ( KnownNat (1+is)
         , KnownNat h
         , B.Binary (Net (NetType h hs os))
         , EvalNorm n
         ) =>
         B.Binary (Net (NetType is ((h :~> n) : hs) os)) where
  put (Layer l ls) = B.put l >> B.put ls
  get =
    do l  <- B.get
       ls <- B.get
       pure (Layer l ls)
