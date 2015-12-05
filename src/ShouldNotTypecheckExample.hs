{-# OPTIONS_GHC -fdefer-type-errors #-}

module ShouldNotTypecheckExample where

-- | Only in Haskell? You can't divide a string by a Bool!
thisWorks :: String
thisWorks =
  fst ("hello", ["world" / True, "!"])

-- | This fails at run time, but only when the lazy return value is completely
-- forced.
thisFails :: String
thisFails =
  snd ("hello", ["world" / True, "!"])
