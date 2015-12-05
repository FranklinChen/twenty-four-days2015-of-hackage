{-# LANGUAGE ScopedTypeVariables #-}

module Sort3Spec where

import qualified Sort3

import Test.Hspec (Spec, hspec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "Sort3" $ do
    prop "sort3 sorts correctly" $ do
      \(triple :: (Int, Int, Int)) ->
        let (a0', a1', a2') = Sort3.sort3 triple
        in a0' <= a1' && a1' <= a2'

main :: IO ()
main = hspec spec
