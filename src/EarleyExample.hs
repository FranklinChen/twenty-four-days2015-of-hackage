{-# LANGUAGE RecursiveDo #-}

module EarleyExample where

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))

-- | Result wanted.
type NumberWord = NonEmpty NumberLetter

-- | 'A' to 'Z'.
type NumberLetter = Char

-- | What to report for something expected.
type Expected = String

grammar :: E.Grammar r (E.Prod r Expected Char NumberWord)
grammar = mdo
  numberWord <- E.rule $
    NonEmpty.cons <$> numberLetter <*> numberWord
    <|> (:| []) <$> numberLetter
  return numberWord

numberLetter :: E.Prod r Expected Char NumberLetter
numberLetter = (Foldable.asum . map numberLetterFor) ['A'..'Z'] <?> "number"

-- | Return a production for a given letter.
--
-- 1 is 'A', 2 is 'B', .. 26 is 'Z'.
numberLetterFor :: NumberLetter -> E.Prod r Expected Char NumberLetter
numberLetterFor c = c <$ E.word (show (toNumber c)) <?> [c]

-- | 'A' is 1, ... 'Z' is 26
toNumber :: NumberLetter -> Int
toNumber c = (Char.ord c - Char.ord 'A') + 1
