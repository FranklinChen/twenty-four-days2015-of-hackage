-- | Extracted from VectorExample for minimal GHC Core output file.
module VectorFusionExample (makeCountsPurely) where

import qualified Data.Word as Word
import qualified Data.Vector.Unboxed as V

-- | Assume 8-bit unsigned integer.
type Value = Word.Word8

-- | Number of occurrences of an 8-bit unsigned value.
-- We assume no overflow beyond 'Int' range.
type CountOfValue = Int

-- | NOT the real thing!! With fake constants 123, 789
-- to help in reading the GHC Core.
makeCountsPurely :: V.Vector Value -> V.Vector CountOfValue
makeCountsPurely =
  V.unsafeAccumulate (+) (V.replicate numPossibleValues 123)
  . V.map (\v -> (fromIntegral v, 789))

-- | 256, in our case.
numPossibleValues :: Int
numPossibleValues = fromIntegral (maxBound :: Value) + 1
