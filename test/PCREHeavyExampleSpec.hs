module PCREHeavyExampleSpec where

import PCREHeavyExample (mediaRegex)

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Regex.PCRE.Heavy ((=~))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      it "has audio" $ do
        "@Media:\thas-audio,   audio" `shouldSatisfy` (=~ mediaRegex)
      it "has video" $ do
        "@Media:\thas-video,video" `shouldSatisfy` (=~ mediaRegex)
      it "has audio but missing" $ do
        "@Media:\thas-audio-but-missing, audio, missing" `shouldSatisfy` (=~ mediaRegex)
      it "has video but unlinked" $ do
        "@Media:\thas-video-but-unlinked  , video,      unlinked" `shouldSatisfy` (=~ mediaRegex)
    describe "no match" $ do
      it "no audio or video" $ do
        "@Media:\tno-audio-or-video" `shouldSatisfy` (not . (=~ mediaRegex))
      it "missing media field" $ do
        "@Media:\tmissing-media-field, unlinked" `shouldSatisfy` (not . (=~ mediaRegex))
