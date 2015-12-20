module DimensionalExample where

import Prelude hiding ((/))
import Numeric.Units.Dimensional.Prelude
       ( (*~)
       , (/)
       , Quantity, Recip, DTime, Length, Time
       , one, minute
       )

-- | "Ideal" turnover for steps while running is 180 steps per minute.
turnover :: Quantity (Recip DTime) Double
turnover = (180 *~ one) / (1 *~ minute)

requiredStrideLength
  :: Length Double
  -> Time Double
  -> Length Double
requiredStrideLength distance goalTime =
  distance / goalTime / turnover
