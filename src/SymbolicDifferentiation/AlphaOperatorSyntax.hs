module SymbolicDifferentiation.AlphaOperatorSyntax where

-- | Variable in an expression.
type Var = String

-- | Precedences for our expression constructors.
infixl 6 `Plus`
infixl 7 `Times`

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
deriv (Plus e1 e2)  v = deriv e1 v `plus` deriv e2 v
deriv (Times e1 e2) v = e1 `times` deriv e2 v
                        `plus`
                        deriv e1 v `times` e2

-- | Precedences for our expression "smart" constructors.
infixl 6 `plus`
infixl 7 `times`

-- | Smart constructor that simplifies while combining subexpressions.
plus :: Exp -> Exp -> Exp
N 0  `plus` e    = e
e    `plus` N 0  = e
N n1 `plus` N n2 = N (n1 + n2)
e1   `plus` e2   = e1 `Plus` e2

-- | Smart constructor that simplifies while combining subexpressions.
times :: Exp -> Exp -> Exp
N 0  `times` _    = N 0
_    `times` N 0  = N 0
N 1  `times` e    = e
e    `times` N 1  = e
N n1 `times` N n2 = N (n1 * n2)
e1   `times` e2   = e1 `Times` e2
