{-# LANGUAGE QuasiQuotes #-}

module SymbolicDifferentiation.AnsiWlPprintSpec where

import qualified SymbolicDifferentiation.Earley as Earley
import qualified SymbolicDifferentiation.AnsiWlPprint as AnsiWlPprint

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)

import Data.String.Here (hereLit)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "anti-wl-pprint for symbolic differentiation expression" $ do
    let eString = [hereLit|(x*1) + 2*y + ((3+z) * (4*b)) * (5+d+w)|]
    let ([e], _) = Earley.parses eString
    it eString $ do
      show (AnsiWlPprint.prettyPrint e) `shouldBe`
        [hereLit|x*1 + 2*y + (3 + z)*4*b*(5 + d
+ w)|]

main :: IO ()
main = hspec spec
