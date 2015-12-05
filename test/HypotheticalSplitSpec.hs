{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional properties for <http://hackage.haskell.org/package/split split> package.
-- Note: split already has <http://hub.darcs.net/byorgey/split/browse/test/ extensive property tests>.
module HypotheticalSplitSpec where

import qualified Data.List.Split as Split

import Test.Hspec (Spec, hspec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "split" $ do
    prop "splitOn always results in nonempty list" $ do
      \subList (list :: String) ->
        Split.splitOn subList list `shouldSatisfy` not . null

main :: IO ()
main = hspec spec
