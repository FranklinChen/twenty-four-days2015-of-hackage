module SymbolicDifferentiation.OperatorSyntaxSpec where

import SymbolicDifferentiation.OperatorSyntax (Exp(N, V, (:+:), (:*:)), deriv)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "symbolic differentiation" $ do
    prop "d/dx (x + n) == 1" $ \x n ->
      deriv (V x :+: N n) x `shouldBe` N 1
    prop "d/dx (x + y) == x, if x /= y" $ \x y ->
      x /= y ==>
      deriv (V x :*: V y) x `shouldBe` V y
    prop "d/dx (a * x + b) == x" $ \a x b ->
      deriv (N a :*: V x :+: N b) x `shouldBe` N a
    it "d/dx (x * y * (x + 3)) == (x * y) + y * (x + 3)" $ do
      deriv (V "x" :*: V "y" :*: (V "x" :+: N 3)) "x" `shouldBe`
        (V "x" :*: V "y") :+: (V "y" :*: (V "x" :+: N 3))

main :: IO ()
main = hspec spec
