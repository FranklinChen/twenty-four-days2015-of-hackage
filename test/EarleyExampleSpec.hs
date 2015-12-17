module EarleyExampleSpec where

import EarleyExample (grammar, NumberWord, Expected)
import qualified Text.Earley as E
import qualified Data.List.NonEmpty as NonEmpty

import Test.Hspec (Spec, hspec, describe, it, shouldMatchList)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "EarleyExample" $ do
    it "returns all possible parses of number words" $ do
      let (result, _) = parseNumberWord 1234
      map NonEmpty.toList result `shouldMatchList`
        ["ABCD", "AWD", "LCD"]

parseNumberWord :: Integer -> ([NumberWord], E.Report Expected String)
parseNumberWord = E.fullParses (E.parser grammar) . show

main :: IO ()
main = hspec spec
