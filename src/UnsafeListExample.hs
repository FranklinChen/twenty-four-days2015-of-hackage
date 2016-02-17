-- | Unsafe code posted <https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/ on Reddit>

module UnsafeListExample where

-- <http://hackage.haskell.org/package/split split> utility library
import qualified Data.List.Split as Split

main :: IO ()
main = do
    contents <- readFile "input.txt"
    if null contents
        then return ()
        else do
            putStrLn $ show (totalArea(parseFile contents))

totalArea :: [(Int, Int, Int)] -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs

partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where areas       = [l, w, h]

        -- 'maximum' is unsafe
        smallSides  = filter (< maximum areas) areas

        -- 'foldl1' is unsafe
        slack       = foldl1 (*) smallSides

parseFile :: String -> [(Int, Int, Int)]
parseFile xs = map (splitDimensions) (breakLines xs)

breakLines :: String -> [String]
breakLines = Split.splitOn "\n"

-- | 'read' is unsafe. '(!!)' is unsafe.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)
