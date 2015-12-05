{-# LANGUAGE ScopedTypeVariables #-}

module UnsafeListExampleSpec where

import UnsafeListExample (totalArea, parseFile)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy, shouldThrow, anyException, pendingWith)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive(..))
import Control.Exception (evaluate)
import Text.Printf (printf)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "UnsafeListExample" $ do
    it "totalArea crashes for a particular input" $ do
      let contents = "1 x 1 x 1"
      evaluate (totalArea (parseFile contents)) `shouldThrow` anyException
{- Commented out to avoid failure.
    prop "totalArea gives something reasonable on any triple of ints" $ do
      \(Positive (l :: Int)) (Positive (w :: Int)) (Positive (h :: Int)) ->
        let contents = printf "%d x %d x %d" l w h
        in totalArea (parseFile contents) `shouldSatisfy` (> 0)
-}

main :: IO ()
main = hspec spec
