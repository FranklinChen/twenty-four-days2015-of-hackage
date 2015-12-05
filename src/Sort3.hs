module Sort3 (sort3) where

-- | Sort exactly 3 values.
-- Logic stolen from <https://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Optimal.html vector-algorithms>
sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (a0, a1, a2) =
  if a0 > a1
  then if a0 > a2
       then if a2 < a1
               then (a2, a1, a0)
               else (a1, a2, a0)
       else (a1, a0, a2)
  else if a1 > a2
       then if a0 > a2
            then (a2, a0, a1)
            else (a0, a2, a1)
       else (a0, a1, a2)
