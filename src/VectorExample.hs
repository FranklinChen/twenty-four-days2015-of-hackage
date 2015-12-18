-- | <http://programmingpraxis.com/2015/09/11/finding-the-median/ Finding the median>
--
-- The median of an array is the value in the middle if the array
-- was sorted; if the array has an odd number of items n, the median
-- is the (n+1)/2’th largest item in the array (which is also the
-- (n+1)/2’th smallest item in the array), and if the array has an
-- even number of items n, the median is the arithmetic average of the
-- n/2’th smallest item in the array and the n/2’th largest item in
-- the array. For instance, the median of the array [2,4,5,7,3,6,1] is
-- 4 and the median of the array [5,2,1,6,3,4] is 3.5.
--
-- Your task is to write a program that takes an array of 8-bit
-- integers (possibly but not necessarily in sort) and finds the
-- median value in the array; you should find an algorithm that takes
-- linear time and constant space. When you are finished, you are
-- welcome to read or run a suggested solution, or to post your own
-- solution or discuss the exercise in the comments below.
--
-- Note: I didn't have to use any of the unsafe functions below.
-- The safe versions are the default, but I wanted to show that
-- you can write C in Haskell if you want.
--
-- Also, all the implementations are sequential, not parallel.

{-# LANGUAGE BangPatterns #-}

module VectorExample where

import qualified Data.Word as Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Algorithms.Radix as Radix

-- | Assume 8-bit unsigned integer.
type Value = Word.Word8

-- | What to return as median when there is one.
type MedianValue = Double

-- | This is an obvious implementation, basically the specification.
-- The use of radix sort makes this linear in time, but a copy of
-- the original array is required.
inefficientSpaceMedian :: V.Vector Value -> Maybe MedianValue
inefficientSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (fromIntegral (atSorted midIndex))
  | otherwise = Just (averageValue (atSorted midIndex)
                                   (atSorted (succ midIndex)))
  where
    len = V.length values
    midIndex = pred (succ len `div` 2)

    -- Make a local mutable copy to sort.
    atSorted = V.unsafeIndex (V.modify Radix.sort values)

-- | Average of two values.
averageValue :: Value -> Value -> MedianValue
averageValue a b = (fromIntegral a + fromIntegral b) / 2

-- | Number of occurrences of an 8-bit unsigned value.
-- We assume no overflow beyond 'Int' range.
type CountOfValue = Int

-- | Create a table of counts for each possible value, since we know
-- the number of values is small and finite.
constantSpaceMedian :: V.Vector Value -> Maybe MedianValue
constantSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (findMid 0 (numToCount - countOf 0))
  | otherwise = Just (findMid2 0 (numToCount - countOf 0))
  where
    len = V.length values

    -- How many sorted elements to count off, to reach first median value.
    numToCount = succ len `div` 2

    -- Make efficient table of counts for each possible value.
    countOf = V.unsafeIndex (makeCountsMutably values)

    findMid !i !numRemaining
      | numRemaining <= 0 = fromIntegral i
      | otherwise = findMid (succ i) (numRemaining - countOf (succ i))

    findMid2 !i !numRemaining =
      case numRemaining `compare` 0 of
        LT -> fromIntegral i -- median is duplicated, don't need average
        GT -> findMid2 (succ i) (numRemaining - countOf (succ i))
        EQ -> midAverage (succ i) (countOf (succ i))
          where
            midAverage j 0 = midAverage (succ j) (countOf (succ j))
            midAverage j _ = averageValue (fromIntegral i) (fromIntegral j)

-- | Use local mutation for efficiency in creating a table of counts,
-- looping through to update it, and freezing the result to return.
makeCountsMutably
  :: V.Vector Value        -- ^ values seen
  -> V.Vector CountOfValue -- ^ value => count
makeCountsMutably values = V.create $ do
  counts <- M.replicate numPossibleValues 0
  V.forM_ values $
     M.unsafeModify counts succ . fromIntegral
  return counts

-- | 256, in our case.
numPossibleValues :: Int
numPossibleValues = fromIntegral (maxBound :: Value) + 1

-- | Make table of counts without using 'MVector'.
makeCountsPurely
  :: V.Vector Value
  -> V.Vector CountOfValue
makeCountsPurely =
  V.unsafeAccumulate (+) (V.replicate numPossibleValues 0)
  . V.map (\v -> (fromIntegral v, 1))
