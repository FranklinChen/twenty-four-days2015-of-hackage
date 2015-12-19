{-# LANGUAGE ScopedTypeVariables #-}

module ListFusionProbeSpec where

import Data.List.Fusion.Probe (fuseThis)

import Test.Hspec ( Spec, hspec, describe, it
                  , shouldBe, shouldSatisfy
                  , shouldThrow, errorCall
                  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Function (Fun(..), apply)
import Control.Exception (evaluate)
import Control.Arrow ((>>>))
import Data.Function ((&))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "list-fusion-probe" $ do
    it "runs a chain of maps, filters" $
      let list1 = ["Hello", "my", "world!"]

          -- ["Hello", "world!"]
          list2 = filter ((> 2) . length) list1

          -- [5, 6]
          list3 = map length list2

          list4 = map (*3) list3
      in list4 `shouldBe` [15, 18]
    it "runs a chain of maps, filters (OO-style)" $
      let list4 = ["Hello", "my", "world!"] &
                     (filter (length >>> (> 2))
                      >>> map length
                      >>> map (*3)
                     )
      in list4 `shouldBe` [15, 18]
    it "runs a chain of maps, filters, written compositionally with >>>" $
      let pipeline = filter (length >>> (> 2))
                     >>> map length
                     >>> map (*3)
      in pipeline ["Hello", "my", "world!"] `shouldBe` [15, 18]
    it "runs a chain of maps, filters, written compositionally with ." $
      let pipeline = map (*3)
                     . map length
                     . filter ((> 2) . length)
      in pipeline ["Hello", "my", "world!"] `shouldBe` [15, 18]
    it "fuses a chain of maps, filters" $
      let list1 = ["Hello", "my", "world!"]

          -- ["Hello", "world!"]
          list2 = fuseThis $ filter ((> 2) . length) list1

          -- [5, 6]
          list3 = fuseThis $ map length list2

          list4 = map (*3) list3
      in list4 `shouldBe` [15, 18]
    it "fuses a chain of maps, filters" $
      let list4 = ["Hello", "my", "world!"]
          pipeline = filter (length >>> (> 2)) >>> fuseThis
                     >>> map length >>> fuseThis
                     >>> map (*3)
      in pipeline list4 `shouldBe` [15, 18]
    it "Prelude foldl fuses" $
      let list = fuseThis [0..1001] :: [Int]
      in foldl (+) 0 list `shouldBe` 501501
    it "handwritten myFoldl fails to fuse" $
      let list = fuseThis [0..1001] :: [Int]
      in evaluate (myFoldl (+) 0 list) `shouldThrow`
           errorCall "fuseThis: List did not fuse"
    prop "Prelude foldl fuses the result of a filter, map pipeline" $
      \(list :: [Int]) (predicate :: Fun Int Bool) (f :: Fun Int Int) ->
      let pipeline = fuseThis . filter (apply predicate)
                     . fuseThis . map (apply f)
      in
         -- Just to force evaluation.
        foldl (+) 0 (pipeline list) `shouldSatisfy` (<= maxBound)

-- | This example taken straight from `list-fusion-probe` tests directory.
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f = go
  where go a [] = a
        go a (x:xs) = go (f a x) xs

main :: IO ()
main = hspec spec
