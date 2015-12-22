module MultilineRe (multilineRe) where

import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (multiline, utf8)
import Language.Haskell.TH.Quote (QuasiQuoter)

multilineRe :: QuasiQuoter
multilineRe = mkRegexQQ [multiline, utf8]
