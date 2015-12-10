{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExpSpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.SExp as SExp

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Data.String.Here (i)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "S-expression syntax for expression" $ do
    prop "(+ x a)" $ \a ->
      SExp.parse [i|(+ x ${a})|] `shouldBe`
        Right (Plus (V "x") (N a))

    prop "(* (+ x a) (+ y b))" $ \a b ->
      SExp.parse [i|
                     (* (+ x ${a})
                        (+ y ${b}))
                   |] `shouldBe`
        Right (Times (Plus (V "x") (N a))
                     (Plus (V "y") (N b)))

    it "(!? x y)" $
      SExp.parse "(!? x y)" `shouldBe`
        Left "\"!?\" is not a valid operator"

    prop "pretty-printing" $ \a b ->
      SExp.prettyPrint (Times (Plus (V "x") (N a))
                              (Plus (V "y") (N b))) `shouldBe`
       [i|(* (+ x ${a}) (+ y ${b}))|]

main :: IO ()
main = hspec spec
