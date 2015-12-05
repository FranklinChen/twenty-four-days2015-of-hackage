-- | Unsafe code posted <https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/ on Reddit> and turned into safe code using NonEmpty.

module NonEmptyListExample where

-- | <http://hackage.haskell.org/package/split split> utility library
import qualified Data.List.Split as Split

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)

-- | Our utility module.
import qualified Sort3

main :: IO ()
main = do
    contents <- readFile "input.txt"
    case NonEmpty.nonEmpty contents of
      Nothing -> return ()
      Just contents1 -> putStrLn $ show (totalArea(parseFile contents1))

totalArea :: NonEmpty (Int, Int, Int) -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs

partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    areas :: NonEmpty Int
    areas = NonEmpty.fromList [l, w, h]

    -- 'maximum' is safe on 'NonEmpty'
    -- But 'smallSides' can be empty because of 'NonEmpty.filter',
    -- and 'NonEmpty.fromList' is unsafe!
    smallSides :: [Int]
    smallSides = NonEmpty.filter (< maximum areas) areas

    -- unsafe!
    smallSides1 :: NonEmpty Int
    smallSides1 = NonEmpty.fromList smallSides

    -- 'foldl1' is safe on 'NonEmpty'
    slack = foldl1 (*) smallSides1

parseFile :: NonEmpty Char -> NonEmpty (Int, Int, Int)
parseFile xs = NonEmpty.map (splitDimensions) (breakLines xs)

-- | We ended up not needing the fact that the input is nonempty, and
-- converted it to a regular list.
breakLines :: NonEmpty Char -> NonEmpty String
breakLines string1 = ourSplitOn "\n" (NonEmpty.toList string1)

-- | 'read' is unsafe. '(!!)' is unsafe.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)

-- | Using unsafe 'NonEmpty.fromList' is safe because we know
-- the result 'from Split.splitOn' is nonempty. Note that the elements
-- themselves can be empty.
ourSplitOn :: Eq a => [a] -> [a] -> NonEmpty [a]
ourSplitOn subList list = NonEmpty.fromList (Split.splitOn subList list)

-- | Don't use lists at all!
bestPartialArea :: (Int, Int, Int) -> Int
bestPartialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    (side0, side1, _) = Sort3.sort3 (l, w, h)
    slack = side0 * side1
