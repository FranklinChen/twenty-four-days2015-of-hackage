{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module SymbolicDifferentiation.EarleySpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.Earley as Earley
import Text.Earley (Report(..))

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative(..))

import Data.String.Here (i)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "Custom syntax for expression parsed by Earley" $ do
    -- For simplicity, don't support negative numeric literals now.
    prop "x + a" $ \(NonNegative (a :: Int)) ->
      fst (Earley.parses [i|x + ${a}|]) `shouldBe`
        [Plus (V "x") (N a)]

    prop "x*a + y*b * (z+c)" $
      \(NonNegative (a :: Int))
       (NonNegative (b :: Int))
       (NonNegative (c :: Int)) ->
      fst (Earley.parses [i|x*${a} + y*${b} * (z+${c})|]) `shouldBe`
        [Plus (Times (V "x") (N a))
              (Times (Times (V "y") (N b))
                     (Plus (V "z") (N c)))]

    it "x + y * + 5" $
      Earley.parses "x + y * + 5" `shouldSatisfy`
        \case
          ([], Report { position = 8
                      , expected = ["number", "identifier", "("]
                      , unconsumed = "+ 5"
                      }) -> True
          _ -> False

main :: IO ()
main = hspec spec
