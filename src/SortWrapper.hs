{-# LANGUAGE RankNTypes #-}

module SortWrapper (Sort(..)) where

newtype Sort =
  Sort { getSort :: forall a. Ord a => [a] -> [a] }
