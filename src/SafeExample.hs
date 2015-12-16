module SafeExample where

import qualified Safe

-- | Crashes if nobody in line.
unsafeReportFirstInLine :: [Int] -> String
unsafeReportFirstInLine nums =
  "next up: customer " ++ show (head nums)

-- | Using 'Safe.headMay' and pattern matching on Maybe.
reportFirstInLine :: [Int] -> String
reportFirstInLine nums =
  case Safe.headMay nums of
    Just num -> "next up: customer " ++ show num
    Nothing -> "there are no customers in line"

-- | Using pattern matching on list.
reportFirstInLine2 :: [Int] -> String
reportFirstInLine2 [] = "there are no customers in line"
reportFirstInLine2 (num:_) = "next up: customer " ++ show num

-- | No pattern matching.
reportFirstInLine3 :: [Int] -> String
reportFirstInLine3 =
  maybe "there are no customers in line" reportOne . Safe.headMay

reportOne :: Int -> String
reportOne num = "next up: customer " ++ show num

-- | Code golf.
reportFirstInLine4 :: [Int] -> String
reportFirstInLine4 =
  maybe "there are no customers in line"
        (("next up: customer " ++) . show)
        . Safe.headMay
