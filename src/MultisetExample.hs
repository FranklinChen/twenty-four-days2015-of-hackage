{-# LANGUAGE OverloadedStrings #-}

module MultisetExample where

import qualified Data.MultiSet as MultiSet
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Ord (Down(..))
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Arrow ((>>>))
import Data.Monoid ((<>))

-- | Break up text into "words" separated by spaces, while treating
-- non-letters as spaces, count them, output a report line for each
-- word and its count in descending order of count but ascending order
-- of word.
wordCount :: LazyText.Text -> LazyText.Text
wordCount =
  LazyText.map replaceNonLetterWithSpace
  >>> LazyText.words
  >>> MultiSet.fromList
  >>> MultiSet.toOccurList
  >>> List.sortOn (snd >>> Down)
  >>> map summarizeWordCount
  >>> mconcat
  >>> LazyBuilder.toLazyText

-- | Same thing, using traditional right-to-left composition.
wordCountTraditional :: LazyText.Text -> LazyText.Text
wordCountTraditional =
  LazyBuilder.toLazyText
  . mconcat
  . map summarizeWordCount
  . List.sortOn (Down . snd)
  . MultiSet.toOccurList
  . MultiSet.fromList
  . LazyText.words
  . LazyText.map replaceNonLetterWithSpace

replaceNonLetterWithSpace :: Char -> Char
replaceNonLetterWithSpace c
  | Char.isLetter c = c
  | otherwise = ' '

summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
