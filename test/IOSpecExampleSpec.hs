{-# LANGUAGE ScopedTypeVariables #-}

module IOSpecExampleSpec where

import IOSpecExample (logIn)
import qualified Test.IOSpec as IO

import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Coerce (coerce)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "IOSpec" $ do
    prop "logIn outputs prompts until secret is guessed" $
      \(notSecretLines :: [NotSecretString]) (anyLines :: [NotNewlineString]) ->
      let allLines = coerce notSecretLines
                     ++ ["secret"]
                     ++ coerce anyLines
          outputLines = ["% Enter password:"]
                        ++ concatMap
                           (const [ "% Wrong password!"
                                  , "% Try again:"])
                           notSecretLines
                        ++ ["$ Congratulations!"]
      in takeOutput (withInput (unlines allLines)
                               (IO.evalIOSpec logIn IO.singleThreaded))
         == unlines outputLines

-- | User input without a newline, and not equal to "secret".
newtype NotSecretString =
  NotSecretString { getNotSecretString :: NotNewlineString }
  deriving (Show)

instance Arbitrary NotSecretString where
  arbitrary = NotSecretString <$>
              arbitrary `suchThat` ((/= "secret") . coerce)
  shrink = map NotSecretString
              . filter ((/= "secret") . coerce)
              . shrink
              . getNotSecretString

type NotNewlineString = [NotNewlineChar]

newtype NotNewlineChar =
  NotNewlineChar { getNotNewlineChar :: Char }
  deriving (Show)

-- | Quick hack. Ideally should write specific generator rather than
-- filtering off the default 'Char' generator.
instance Arbitrary NotNewlineChar where
  arbitrary = NotNewlineChar <$>
              arbitrary `suchThat` (/= '\n')
  shrink = map NotNewlineChar
              . filter (/= '\n')
              . shrink
              . getNotNewlineChar

takeOutput :: IO.Effect () -> String
takeOutput (IO.Done _) = ""
takeOutput (IO.Print c xs) = c : takeOutput xs
takeOutput _ = error "takeOutput: expects only Done, Print"

withInput :: [Char] -> IO.Effect a -> IO.Effect a
withInput _ (IO.Done x) = IO.Done x
withInput stdin (IO.Print c e) = IO.Print c (withInput stdin e)
withInput (char:stdin) (IO.ReadChar f) = withInput stdin (f char)
withInput _ _ = error "withInput: expects only Done, Print, ReadChar"

-- | Without coerce, we'd need functions such as these.
notSecretStringsAsStrings :: [NotSecretString] -> [String]
notSecretStringsAsStrings = map notSecretStringAsString

notSecretStringAsString :: NotSecretString -> String
notSecretStringAsString = map getNotNewlineChar . getNotSecretString

main :: IO ()
main = hspec spec
