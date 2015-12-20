module DimensionalExampleSpec where

import DimensionalExample (requiredStrideLength)

import Prelude hiding ((+))
import Numeric.Units.Dimensional.Prelude
       ( (*~), (/~)
       , (+)
       , Length, Time, kilo, meter, minute, second
       )
import Numeric.Units.Dimensional.NonSI (foot)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "dimensional" $ do
    it "check required running stride length" $
      let fiveK :: Length Double
          fiveK = 5 *~ kilo meter

          goalTime :: Time Double
          goalTime = 24 *~ minute + 45 *~ second

          feetPerStep :: Double
          feetPerStep = requiredStrideLength fiveK goalTime /~ foot
      in feetPerStep `shouldSatisfy` (\x -> x > 3 && x < 4)

main :: IO ()
main = hspec spec
