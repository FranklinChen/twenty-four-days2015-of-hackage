module SymbolicDifferentiation.OperatorSyntax where

-- | Variable in an expression.
type Var = String

-- | Precedences for our expression constructors.
infixl 6 :+:
infixl 7 :*:

-- | Expression.
data Exp
  = N Int        -- ^ number
  | V Var        -- ^ variable
  | Exp :+: Exp  -- ^ sum
  | Exp :*: Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivative of expression with respect to a variable.
deriv :: Exp -> Var -> Exp
deriv (N _)       _ = N 0
deriv (V x )      y = N (if x == y then 1 else 0)
deriv (e1 :+: e2) v = deriv e1 v .+. deriv e2 v
deriv (e1 :*: e2) v = e1 .*. deriv e2 v
                      .+.
                      deriv e1 v .*. e2

-- | Precedences for our expression "smart" constructors.
infixl 6 .+.
infixl 7 .*.

-- | Smart constructor that simplifies while combining subexpressions.
(.+.) :: Exp -> Exp -> Exp
N 0  .+. e    = e
e    .+. N 0  = e
N n1 .+. N n2 = N (n1 + n2)
e1   .+. e2   = e1 :+: e2

-- | Smart constructor that simplifies while combining subexpressions.
(.*.) :: Exp -> Exp -> Exp
N 0  .*.  _   = N 0
_    .*. N 0  = N 0
N 1  .*. e    = e
e    .*. N 1  = e
N n1 .*. N n2 = N (n1 * n2)
e1   .*. e2   = e1 :*: e2
