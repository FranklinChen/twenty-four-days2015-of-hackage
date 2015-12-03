module PCREHeavyExampleSpec where

import Test.Hspec

import PCREHeavyExample (mediaRegex)

import Text.Regex.PCRE.Heavy ((=~))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      mapM_ matchSpec matchExamples
    describe "no match" $ do
      mapM_ nonMatchSpec nonMatchExamples

matchSpec :: (String, String) -> Spec
matchSpec (description, text) =
  it description $do
    text `shouldSatisfy` (=~ mediaRegex)

nonMatchSpec :: (String, String) -> Spec
nonMatchSpec (description, text) =
  it description $do
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
