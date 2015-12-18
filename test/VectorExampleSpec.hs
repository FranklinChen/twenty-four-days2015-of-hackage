module VectorExampleSpec where

import VectorExample
       ( Value, MedianValue, averageValue
       , inefficientSpaceMedian, constantSpaceMedian
       )

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Radix as Radix

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary, shrink), choose, sized, shrinkList)

-- | Required for auto-discovery.
spec :: Spec
spec = do
  describe "compute median of vector of 8-bit unsigned integers" $ do
    medianSpec "inefficientSpaceMedian" inefficientSpaceMedian
    medianSpec "constantSpaceMedian" constantSpaceMedian

medianSpec :: String
            -> (V.Vector Value -> Maybe MedianValue)
            -> Spec
medianSpec description findMedian =
  describe description $ do
    describe "some examples" $ do
      it "handles odd number of elements" $ do
        findMedian (V.fromList [2, 4, 5, 7, 3, 6, 1]) `shouldBe` Just 4.0
      it "handles nonzero even number of elements" $ do
        findMedian (V.fromList [5, 2, 1, 6, 3, 4]) `shouldBe` Just 3.5
    describe "properties" $ do
      it "handles no elements" $ do
        findMedian V.empty `shouldBe` Nothing
      prop "handles one element" $ \v ->
        findMedian (V.singleton v) == Just (fromIntegral v)
      prop "handles odd number of elements" $
        \(VectorWithOdd values) ->
          let len = V.length values
              midIndex = pred (succ len `div` 2)
              sorted = V.modify Radix.sort values
          in findMedian values == Just (fromIntegral (sorted ! midIndex))
      prop "handles positive even number of elements" $
        \(VectorWithPositiveEven values) ->
          let len = V.length values
              midIndex = pred (succ len `div` 2)
              sorted = V.modify Radix.sort values
          in findMedian values ==
            Just (averageValue (fromIntegral (sorted ! midIndex))
                               (sorted ! succ midIndex))

-- | Generate a vector with an odd number of elements.
newtype VectorWithOdd a =
  VectorWithOdd { getVectorWithOdd :: V.Vector a }
  deriving (Show)

instance (Arbitrary a, V.Unbox a) => Arbitrary (VectorWithOdd a) where
  arbitrary = sized $ \n -> do
    k <- choose (0, n `div` 2)
    VectorWithOdd <$> V.replicateM (2*k+1) arbitrary
  shrink = map (VectorWithOdd . V.fromList)
           . shrinkList shrink
           . V.toList
           . getVectorWithOdd

-- | Generate a vector with a nonzero even number of elements.
newtype VectorWithPositiveEven a =
  VectorWithPositiveEven { getVectorWithPositiveEven :: V.Vector a }
  deriving (Show)

instance (Arbitrary a, V.Unbox a) => Arbitrary (VectorWithPositiveEven a) where
  arbitrary = sized $ \n -> do
    k <- choose (1, 1 `max` (n `div` 2))
    VectorWithPositiveEven <$> V.replicateM (2*k) arbitrary
  shrink = map (VectorWithPositiveEven . V.fromList)
           . shrinkList shrink
           . V.toList
           . getVectorWithPositiveEven

main :: IO ()
main = hspec spec
