{-# Language GADTs, DataKinds, ScopedTypeVariables #-}
import LA
import Act

type V n                  = Vector n Scalar
type IGradient n          = V n             -- ^ Error change wrt. input
type WGradient n          = V (1 + n)       -- ^ Error change wrt. weight
type Neuron n             = V (1 + n)       -- ^ Weights of a neuron

-- | Various normalization (aka. activation) functions.
data Norm = Sigmoid | Softmax | ReLU

-- | General topology of a neural net.
data NetTopology layer inputs hidden outputs where
  (:>)    :: layer inputs size ->
             NetTopology layer size hidden outputs ->
             NetTopology layer inputs (size : hidden) outputs
  Result  :: NetTopology layer n '[] n


-- | A layer of the network.
data Layer ins outs = Layer
  { neurons   :: Vector outs (Neuron ins) -- ^ Weights for the layer
  , norm      :: Norm                     -- ^ How to normalize the outputs.
  }

-- | Gradient of a layer.
newtype GLayer ins outs = GLayer
  { neuronGs  :: Vector outs (WGradient ins)
  }


type Net  = NetTopology Layer
type NetG = NetTopology GLayer

--------------------------------------------------------------------------------
-- Evaluation

-- | Linear part of a neuron.
evalNeuron :: Neuron ins -> V ins -> Scalar
evalNeuron ws ins = sum (pointwise (*) (push 1 ins) ws)

-- | Evaluate a layer.
evalLayer :: Layer ins outs -> V ins -> V outs
evalLayer l is = evalNorm (norm l) ((`evalNeuron` is) <$> neurons l)

-- | Normalization functions.
evalNorm :: Norm -> V n -> V n
evalNorm fun ins =
  case fun of
    Sigmoid -> sigmoid <$> ins
    Softmax -> softmax ins
    ReLU    -> relu <$> ins

-- | Evaluate a network.
evalNet :: Net ins hs outs -> V ins -> V outs
evalNet net =
  case net of
    Result    -> id
    l :> next -> evalNet next . evalLayer l

--------------------------------------------------------------------------------
-- Gradients

-- | Compute how the loss will change for a given input-output pair,
-- if we change any of the weights.
netG :: forall ins hs outs.
  Net ins hs outs -> V ins -> V outs -> NetG ins hs outs
netG net0 is0 expected =
  case net0 of
    Result    -> Result
    l :> more -> fst (layerG is0 l more)

  where
  layerG :: V a -> Layer a b -> Net b h outs -> (NetG a (b:h) outs, IGradient b)
  layerG is l next = (wGradients :> newNext, igs)
    where
    os             = evalLayer l is
    wGradients     = GLayer ((.* push 1 is) <$> igs)

    (newNext, igs) =
      case next of
        Result -> (Result, pointwise delta os expected)
          where
          delta a e =
            case norm l of
              Sigmoid -> (a - e) * a * (1 - a)  -- Square Error Loss
              Softmax -> a - e                  -- Cross Entropy Loss,
                                                -- assuming `sum e == 1`
              ReLU    -> if e < a then 1 else 0 -- Square Error Loss

        l' :> next' -> (newNet, updateIGradient l' (norm l) os igs')
          where
          (newNet, igs') = layerG os l' next'


updateIGradient ::
  Layer ins outs                                              ->
  Norm              {- ^ How our inputs were normalized -}    ->
  V ins             {- ^ *Normalized* inputs to this layer -} ->
  IGradient outs    {- ^ How changes to our linear outputs affect the loss,
                         (i.e., the derivative of the normalization function
                         is included). -}                      ->
  IGradient ins     {- ^ How changes to our unnormalized inputs affect the loss
                         (i.e., the derivative of the previous normalization
                          function is included). -}
updateIGradient l f is ogs =
  case f of
    ReLU    -> pointwise dReLU    is  gradients
    Sigmoid -> pointwise dSigmoid is  gradients
    Softmax -> pointwise dSoftmax is' gradients
  where
  dReLU o xs        = if o < 0 then 0 else sum xs
  dSigmoid o xs     = sum xs * o * (1 - o)

  is'               = indexed is
  -- XXX: A bunch of repeated multiplications here
  dSoftmax o xs     = sum xs * sum (d o <$> is')
    where
    d (i,x) (j,y)
      | i == j    = x * (1 - x)
      | otherwise = - x * y

  -- The `pop` is to skip the weight of the bias input
  gradients = transpose (pointwise (\x y -> x .* pop y) ogs (neurons l))

--------------------------------------------------------------------------------
-- Updating the weights

updateLayer :: Scalar -> Layer ins outs -> GLayer ins outs -> Layer ins outs
updateLayer eta l lg =
  l { neurons = mPointwise updateWeight (neurons l) (neuronGs lg) }
  where
  updateWeight w g = w - eta * g

updateNet ::
  Scalar -> Net ins hs outs -> Scalar -> NetG ins hs outs -> Net ins hs outs
updateNet eta net0 samples g0
  | samples == 0 = net0
  | otherwise    = go net0 g0
  where
  eta' = eta / samples

  go :: Net a h b -> NetG a h b -> Net a h b
  go net g =
    case (net,g) of
      (Result,Result)          -> Result
      (l :> more, lg :> moreG) -> updateLayer eta' l lg :> go more moreG



