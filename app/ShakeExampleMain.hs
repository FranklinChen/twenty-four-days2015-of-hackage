{-# LANGUAGE QuasiQuotes #-}

module Main where

import MultilineRe (multilineRe)

import Development.Shake
import Development.Shake.FilePath

import Text.Regex.PCRE.Heavy (Regex, scan)
import Data.Maybe (listToMaybe)
import Text.Printf (printf)
import Control.Monad (zipWithM)

shakeDir :: FilePath
shakeDir = "_build"

-- | File, sorted by day, containing a line for each Markdown source of a post.
daysSources :: FilePath
daysSources = shakeDir </> "days-sources"

-- | File, sorted by day, containing a line for each generated Day of Hackage post.
daysUrls :: FilePath
daysUrls = shakeDir </> "days-urls"

tocFile :: FilePath
tocFile = shakeDir </> "TOC.md"

-- | Base directory of blog source.
blogDir :: FilePath
blogDir = "/Users/chen/Sync/ConscientiousProgrammer"

-- | Base directory of generated blog.
publicDir :: FilePath
publicDir = "/Users/chen/ConscientiousProgrammer-public"

-- | Generated HTML directory for each post.
urlsGlob :: FilePattern
urlsGlob = "blog/2015/1*/*/*hackage-2015-day-*"

-- | Location of Markdown blog posts.
postDir :: FilePath
postDir = blogDir </> "content/post"

-- | Rely on naming convention here.
postGlob :: FilePattern
postGlob = "*hackage-2015-day-*"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=shakeDir} $ do
  want [tocFile]

  tocFile %> \out -> do
    sourcePaths <- readFileLines daysSources
    urls <- readFileLines daysUrls

    toc <- zipWithM extractTOCEntry sourcePaths urls
    writeFileChanged out $ unlines $ map formatTOCEntry toc

  -- Run Hugo to generate a directory for each post.
  daysUrls %> \out -> do
    need [daysSources]
    unit $ cmd (Cwd blogDir) "hugo"
    Stdout stdout <- cmd Shell (Cwd publicDir) "echo" urlsGlob
    writeFileChanged out $ unlines $ words stdout

  daysSources %> \out -> do
    daysFiles <- getDirectoryFiles postDir [postGlob]
    writeFileChanged out $ unlines $ map (postDir </>) daysFiles

-- | A day's entry in the TOC.
data TOCEntry =
  TOCEntry { _day :: Int
           , _title :: String
           , _url :: String
           }
  deriving (Eq, Ord)

dayTitleRegex :: Regex
dayTitleRegex = [multilineRe|^title:.*day\s+(\d+):\s*([^"]+)|]

extractTOCEntry :: FilePath -> String -> Action TOCEntry
extractTOCEntry sourcePath url = do
  text <- readFile' sourcePath
  case listToMaybe (scan dayTitleRegex text) of
    Just (_, [dayString, title]) ->
      return $ TOCEntry (read dayString) title url
    _ ->
      error $ printf "failed to extract day and title from %s" sourcePath

formatTOCEntry :: TOCEntry -> String
formatTOCEntry entry =
  printf "- Day %d: [%s](/%s/)" (_day entry) (_title entry) (_url entry)
