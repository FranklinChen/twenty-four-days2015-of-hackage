{-# LANGUAGE OverloadedStrings #-}

module MultisetExampleSpec where

import MultisetExample (wordCount)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "multiset" $ do
    it "wordCount" $ do
      let input = "I have so   many words; words I so like to have words for!?"
      let expected = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      wordCount input `shouldBe` expected

main :: IO ()
main = hspec spec
