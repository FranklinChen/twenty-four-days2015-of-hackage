{-# LANGUAGE RankNTypes #-}

module MissingHExtraExampleSpec where

-- | From MissingH
import qualified Data.List.Utils as ListUtils

-- | From extra
import qualified Data.List.Extra as ListExtra

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "MissingH and extra" $ do
    describeReplace "MissingH" ListUtils.replace
    describeReplace "extra" ListExtra.replace

describeReplace
  :: String  -- ^ description
  -> (forall a. Eq a => [a] -> [a] -> [a] -> [a])  -- ^ replace
  -> Spec
describeReplace description replace =
  describe description $ do
    it "replaces all substrings within a string" $ do
      replace "abc" "d" "123abc123abc" `shouldBe` "123d123d"
    it "replaces all int sublists within an int list" $ do
      replace [0 :: Int, 1] [100, 101, 102] [0, 1, 2, 3, 0, 0, 1, 4]
        `shouldBe` [100, 101, 102, 2, 3, 0, 100, 101, 102, 4]

main :: IO ()
main = hspec spec
