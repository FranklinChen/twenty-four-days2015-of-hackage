module EarleyExampleSpec where

import EarleyExample (grammar, NumberWord, Expected)
import qualified Text.Earley as E
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "EarleyExample" $ do
    it "returns all possible parses of number words" $ do
      parseNumberWord 1234 `shouldSatisfy` \(result, _) ->
        List.sort result ==
        List.sort [ NonEmpty.fromList "ABCD"
                  , NonEmpty.fromList "AWD"
                  , NonEmpty.fromList "LCD"
                  ]

parseNumberWord :: Integer -> ([NumberWord], E.Report Expected String)
parseNumberWord = E.fullParses (E.parser grammar) . show

main :: IO ()
main = hspec spec
