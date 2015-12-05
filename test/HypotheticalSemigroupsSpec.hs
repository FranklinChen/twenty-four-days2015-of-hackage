{-# LANGUAGE ScopedTypeVariables #-}

-- Part of a hypothetical test module for the semigroups package.
module HypotheticalSemigroupsSpec where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup ((<>))

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Maybe as Maybe

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "semigroups" $ do
    describe "Data.List.NonEmpty" $ do
      describe "constructor NonEmpty.nonEmpty" $ do
        it "fails on trying to construct from an empty regular list" $ do
          NonEmpty.nonEmpty ([] :: [Int]) `shouldBe` Nothing
        prop "succeeds on any nonempty list" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            NonEmpty.nonEmpty xs `shouldSatisfy` Maybe.isJust
      describe "conversion to regular list" $ do
        prop "converts back to the original regular list" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            let Just nonEmptyXs = NonEmpty.nonEmpty xs
            in NonEmpty.toList nonEmptyXs `shouldBe` xs
    describe "Data.Semigroup.Semigroup" $ do
      prop "<> is associative for String" $ do
        \(x :: String) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)

main :: IO ()
main = hspec spec
