{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module SymbolicDifferentiation.Earley where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative (many, some, (<|>))
import qualified Data.Char as Char
import Control.Monad.ST (ST)
import Data.ListLike (ListLike)

-- | What to report for something expected.
type Expected = String

-- | Return a list of all possible `Exp` parses, and also a status report
-- regardless of how many successes.
parses :: String -> ([Exp], E.Report Expected String)
parses = E.fullParses expParser

-- | Parser created from the grammar.
expParser :: ListLike input Char =>
             ST state (input -> ST state (E.Result state Expected input Exp))
expParser = E.parser grammar

-- | Basically taken from <https://github.com/ollef/Earley/blob/master/examples/Expr2.hs Earley example expression parser>
grammar :: forall r. E.Grammar r (E.Prod r Expected Char Exp)
grammar = mdo
  whitespace <- E.rule $
    many $ E.satisfy Char.isSpace

  let token :: E.Prod r Expected Char a -> E.Prod r Expected Char a
      token p = whitespace *> p

      sym x   = token $ E.symbol x <?> [x]

      ident   = token $ (:) <$> E.satisfy Char.isAlpha
                            <*> many (E.satisfy Char.isAlphaNum)
                            <?> "identifier"
      num     = token $ some (E.satisfy Char.isDigit) <?> "number"
      -- For now, just handle unsigned numeric literals.

  atom <- E.rule $
    (N . read) <$> num
    <|> V <$> ident
    <|> sym '(' *> term <* sym ')'

  factor <- E.rule $
    Times <$> factor <* sym '*' <*> atom
    <|> atom

  term <- E.rule $
    Plus <$> term <* sym '+' <*> factor
    <|> factor

  return $ term <* whitespace
