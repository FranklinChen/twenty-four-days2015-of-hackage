module SymbolicDifferentiation.AlphaSyntax where

-- | Variable in an expression.
type Var = String

-- | Expression.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivative of expression with respect to a variable.
deriv :: Exp -> Var -> Exp
deriv (N _)         _ = N 0
deriv (V v')        v = N (if v' == v then 1 else 0)
deriv (Plus e1 e2)  v = plus (deriv e1 v) (deriv e2 v)
deriv (Times e1 e2) v = plus (times e1 (deriv e2 v))
                             (times (deriv e1 v) e2)

-- | Smart constructor that simplifies while combining subexpressions.
plus :: Exp -> Exp -> Exp
plus (N 0)  e      = e
plus e      (N 0)  = e
plus (N n1) (N n2) = N (n1 + n2)
plus e1     e2     = Plus e1 e2

-- | Smart constructor that simplifies while combining subexpressions.
times :: Exp -> Exp -> Exp
times (N 0)  _      = N 0
times _      (N 0)  = N 0
times (N 1)  e      = e
times e      (N 1)  = e
times (N n1) (N n2) = N (n1 * n2)
times e1     e2     = Times e1 e2
