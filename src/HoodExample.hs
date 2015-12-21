-- | Based on MultisetExample.

{-# LANGUAGE OverloadedStrings #-}

module HoodExample where

-- | hood
import Debug.Hood.Observe (Observable(..), observe, observeBase, printO)

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
wordCount = observe "(1) wordCount"
  >>> LazyText.map replaceNonLetterWithSpace >>> observe "(2) map replaceNonLetterWithSpace"
  >>> LazyText.words             >>> observe "(3) LazyText.words"
  >>> MultiSet.fromList          >>> observe "(4) MultiSet.fromList"
  >>> MultiSet.toOccurList       >>> observe "(5) MultiSet.toOccurList"
  >>> List.sortOn (snd >>> Down) >>> observe "(6) List.sortOn (snd >>> Down)"
  >>> map summarizeWordCount     >>> observe "(7) map summarizeWordCount"
  >>> mconcat                    >>> observe "(8) mconcat"
  >>> LazyBuilder.toLazyText     >>> observe "(9) LazyBuilder.toLazyText"

replaceNonLetterWithSpace :: Char -> Char
replaceNonLetterWithSpace c
  | Char.isLetter c = c
  | otherwise = ' '

summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"

-- | Some orphan instances of 'Observable'.
instance Observable LazyText.Text where
  observer = observeBase
instance (Observable a, Show a) => Observable (MultiSet.MultiSet a) where
  observer = observeBase
instance Observable LazyBuilder.Builder where
  observer = observeBase

exampleRun :: IO ()
exampleRun = printO $
  wordCount "I have all-too-many words; words I don't like much!"
