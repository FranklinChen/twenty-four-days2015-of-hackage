{-# LANGUAGE QuasiQuotes #-}

module PCREHeavyExample where

import Text.Regex.PCRE.Heavy
import Data.Maybe (listToMaybe)
import Text.Printf (printf)
import Control.Monad (forM_)
import Control.Arrow ((>>>))

-- | Match a media name, audio/video, and optional missing/unlinked.
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]

data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

-- | Extract information about a media file if it is present.
extractIfPresent :: (String, [String]) -> Info
extractIfPresent (_, [name, "audio"]) = Audio name
extractIfPresent (_, [name, "video"]) = Video name
extractIfPresent (_, _) = Skip

-- | Output a report.
reportOnInfo :: Maybe Info -> IO ()
reportOnInfo Nothing = putStrLn "no match"
reportOnInfo (Just Skip) = putStrLn "match, but missing or unlinked"
reportOnInfo (Just (Audio path)) = printf "audio at %s\n" path
reportOnInfo (Just (Video path)) = printf "video at %s\n" path

-- | Driver, in traditional right-to-left syntax.
main :: IO ()
main = do
  s <- getContents
  mapM_ (reportOnInfo
        . fmap extractIfPresent
        . listToMaybe
        . scan mediaRegex
       ) (lines s)

-- | In left-to-right syntax that I prefer.
myMain :: IO ()
myMain = do
  s <- getContents
  lines s `forM_` (
    scan mediaRegex
    >>> listToMaybe
    >>> fmap extractIfPresent
    >>> reportOnInfo
    )
