{-# LANGUAGE QuasiQuotes #-}

module HereExampleSpec where

import Data.String.Here (hereLit, here, hereFile, i)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "here" $ do
    it "makes trimmed multi-line strings prettier" $ do
      -- In this case we want trimming.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1"
      let trimmedHereDoc = [here|
words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      trimmedHereDoc `shouldBe` original
    it "makes literal multi-line strings prettier" $ do
      -- In this case assume we want the trailing newline.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      let literalHereDoc = [hereLit|words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      literalHereDoc `shouldBe` original
    it "allows file embed" $ do
      [hereFile|test/Spec.hs|] `shouldBe`
        "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"
    it "makes interpolation prettier" $ do
      let list = [1 :: Int, 2]
      let num = 42 :: Int
      [i|number: ${num}, stuff: ${map (+1) list}|] `shouldBe`
        "number: 42, stuff: [2,3]"

main :: IO ()
main = hspec spec
