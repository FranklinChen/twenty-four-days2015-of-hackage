module SymbolicDifferentiation.AnsiWlPprint where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import Text.PrettyPrint.ANSI.Leijen
       ( Doc
       , int, text, char
       , (</>), (<//>), (<+>), (<>)
       , parens
       )

-- | Very primitive, for illustration only!
--
-- Spaces for sums, but no spaces for products.
-- Soft breaks before operators.
prettyPrint :: Exp -> Doc
prettyPrint e = p 10 e

-- | Pretty-print inside a precedence context to avoid parentheses.
-- Consider + to be 6, * to be 7.
p :: Int -> Exp -> Doc
p _ (N n) = int n
p _ (V v) = text v
p prec (Plus e1 e2) = maybeParens (prec < 7)
  (p 7 e1 </> char '+' <+> p 7 e2)
p prec (Times e1 e2) = maybeParens (prec < 6)
  (p 6 e1 <//> char '*' <> p 6 e2)

maybeParens :: Bool -> Doc -> Doc
maybeParens True = parens
maybeParens False = id
