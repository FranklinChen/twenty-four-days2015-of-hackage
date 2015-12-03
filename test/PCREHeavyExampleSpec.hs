module PCREHeavyExampleSpec where

import PCREHeavyExample (mediaRegex)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)
import Text.Regex.PCRE.Heavy ((=~))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      mapM_ matchSpec matchExamples
    describe "no match" $ do
      mapM_ nonMatchSpec nonMatchExamples

matchSpec :: (String, String) -> Spec
matchSpec (description, text) =
  it description $ do
    text `shouldSatisfy` (=~ mediaRegex)

nonMatchSpec :: (String, String) -> Spec
nonMatchSpec (description, text) =
  it description $ do
    text `shouldSatisfy` (not . (=~ mediaRegex))

matchExamples :: [(String, String)]
matchExamples =
  [ ( "has audio"
    , "@Media:\thas-audio,   audio"
    )
  , ( "has video"
    , "@Media:\thas-video,video"
    )
  , ( "has audio but missing"
    , "@Media:\thas-audio-but-missing, audio, missing"
    )
  , ( "has video but unlinked"
    , "@Media:\thas-video-but-unlinked  , video,      unlinked"
    )
  ]

nonMatchExamples :: [(String, String)]
nonMatchExamples =
  [ ( "no audio or video"
    , "@Media:\tno-audio-or-video"
    )
  , ( "missing media field"
    , "@Media:\tmissing-media-field, unlinked"
    )
  ]

-- | Just for our convenience to manually run just this module's tests.
main :: IO ()
main = hspec spec
