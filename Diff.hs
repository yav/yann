module Diff where

import Data.Ratio(numerator,denominator)

type Name = String

data Expr =
    Expr :+: Expr | Neg   | Zero
  | Expr :*: Expr | Recip | One
  | Expr :.: Expr         | Id
  | Exp           | Ln
    deriving Show

eval :: Expr -> Double -> Double
eval expr =
  case expr of
    f :+: g -> \x -> eval f x + eval g x
    Neg     -> negate
    Zero    -> const 0
    f :*: g -> \x -> eval f x * eval g x
    Recip   -> recip
    One     -> const 1
    f :.: g -> eval f . eval g
    Id      -> id
    Exp     -> exp
    Ln      -> log

instance Num Expr where
  Zero + y  = y
  x + Zero  = x
  x + y     = x :+: y

  negate Zero        = Zero
  negate (Neg :.: e) = e
  negate x           = Neg `o` x

  Zero * _    = Zero
  One  * y    = y
  _    * Zero = Zero
  x    * One  = x
  (Neg :.: x) * y = - (x * y)
  x * (Neg :.: y)  = - (x * y)

  x    * y    = x :*: y

  fromInteger n =
    case compare n 0 of
      EQ -> Zero
      GT -> One + fromInteger (n-1)
      LT -> Neg `o` fromInteger (-n)

  abs    = error "abs unsupported"
  signum = error "signum unsupported"

instance Fractional Expr where
  recip Recip  = Id     -- as long as `x` is not 0
  recip x = Recip `o` x

  x / y = x * recip y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance Floating Expr where
  exp = (Exp :.:)
  log = (Ln  :.:)

  pi  = error "pi"
  sin = error "sin"
  asin = error "asin"
  sinh = error "sinh"
  asinh = error "asinh"

  cos = error "cos"
  acos = error "acos"
  cosh = error "cosh"
  acosh = error "acosh"

  atan = error "atan"
  atanh = error "atanh"

pow :: Int -> Expr
pow n =
  case compare n 0 of
    EQ -> One
    GT -> Id * pow (n-1)
    LT -> Recip :.: pow (-n)

o :: Expr -> Expr -> Expr
Zero `o` _ = Zero
One  `o` _ = One

Id `o` g = g
f `o` Id = f

Neg `o` Neg = Id
Neg `o` (Neg :.: x) = x
(x :.: Neg) `o` Neg = x

Recip `o` Recip = Id        -- except for 0
Recip `o` (Recip :.: x) = x

f `o` g  = f :.: g


diff :: Expr -> Expr
diff expr =
  case expr of
    x :+: y -> diff x + diff y
    Neg     -> -1
    Zero    -> 0
    x :*: y -> diff x * y + x * diff y
    Recip   -> -(1 / pow(-2))
    One     -> 0
    x :.: y -> (diff x `o` y) * diff y
    Id      -> 1
    Exp     -> Exp
    Ln      -> Recip

example :: Expr
example = 1 / (1 + exp (-x))
  where x = Id
