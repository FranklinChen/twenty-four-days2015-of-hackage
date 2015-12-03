module PCREHeavyExampleSpec where

import PCREHeavyExample (mediaRegex)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)
import Text.Regex.PCRE.Heavy ((=~))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describePredicate "pcre-heavy"
    ("match", (=~ mediaRegex))
    (matchExamples, nonMatchExamples)

describePredicate :: Show a =>
     String                           -- ^ description
  -> (String, a -> Bool)              -- ^ (base description, predicate)
  -> ( [(String, a)], [(String, a)] ) -- ^ positive and negative examples
  -> Spec
describePredicate description
                  (baseDescription, predicate)
                  (positiveExamples, negativeExamples) =
  describe description $ do
    describe baseDescription $ do
      mapM_ (predSpec predicate) positiveExamples
    describe ("not " ++ baseDescription) $ do
      mapM_ (predSpec (not . predicate)) negativeExamples

predSpec :: Show a => (a -> Bool) -> (String, a) -> Spec
predSpec predicate (description, a) =
  it description $ do
    a `shouldSatisfy` predicate

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
