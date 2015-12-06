{-# OPTIONS_GHC -fdefer-type-errors #-}

module ShouldNotTypecheckExampleSpec where

import ShouldNotTypecheckExample (thisWorks, thisFails)

import Test.Hspec ( Spec, hspec, describe, it
                  , shouldBe
                  , shouldThrow, anyException
                  )
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.Exception (evaluate)
import Control.DeepSeq (force)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "should-not-typecheck" $ do
    it "should not allow mapping negation over a list of strings" $ do
      shouldNotTypecheck (map not ["hello", "world"])
    it "you can run code even if it contains ill-typed parts" $ do
      thisWorks `shouldBe` "hello"
    it "deferred type errors are only lazily reached" $ do
      length thisFails `shouldBe` 2
    it "deferred type errors cause an exception only when reached" $ do
      evaluate (force thisFails) `shouldThrow` anyException

main :: IO ()
main = hspec spec
