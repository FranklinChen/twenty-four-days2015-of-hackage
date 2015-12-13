module HintExampleSpec where

import SortWrapper (Sort(Sort))
import HintExample (loadSort)

import qualified Language.Haskell.Interpreter as I

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "hint" $ do
    it "dynamically loads a correct polymorphic sort function" $ do
      Right (Sort ourSort) <-
        I.runInterpreter (loadSort "OurSorter" "ourSort")
      ourSort "ebcad" `shouldBe` "abcde"
      ourSort [1 :: Int, 5, 4, 3, 7] `shouldBe` [1, 3, 4, 5, 7]
    it "dynamically loads a wrong (only head) sort function" $ do
      Right (Sort onlyHead) <-
        I.runInterpreter (loadSort "OurSorter" "onlyHead")
      onlyHead "ebcad" `shouldBe` "e"
      onlyHead [True, False] `shouldBe` [True]

main :: IO ()
main = hspec spec
