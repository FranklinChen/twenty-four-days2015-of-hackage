module OurSorter where

import SortWrapper (Sort(..))
import qualified Data.List as List

ourSort :: Sort
ourSort = Sort List.sort

onlyHead :: Sort
onlyHead = Sort (\(x:_) -> [x])
