{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExp where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Data.SCargot as S
import Data.SCargot.Language.Basic (basicParser)
import Data.SCargot.Repr.WellFormed
       (WellFormedSExpr(WFSList, WFSAtom), fromWellFormed)

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Read (signed, decimal)

import Data.String.Here (i)

-- | Error when parsing.
type Error = String

-- | For simplicity, we use 'basicParser' which just treats every atom
-- as 'Text', which we parse later rather than up front.
parse :: Text -> Either Error Exp
parse text = parseOneSexp text >>= toExp

parseOneSexp :: Text -> Either Error (WellFormedSExpr Text)
parseOneSexp = S.decodeOne (S.asWellFormed basicParser)

toExp :: WellFormedSExpr Text -> Either Error Exp
toExp (WFSAtom text) = fromAtom text
toExp (WFSList [WFSAtom operatorText, sexp1, sexp2]) = do
  operator <- fromOperator operatorText
  e1 <- toExp sexp1
  e2 <- toExp sexp2
  return (operator e1 e2)
toExp list@(WFSList _) = Left [i|${list} should have exactly 3 elements|]

fromOperator :: Text -> Either Error (Exp -> Exp -> Exp)
fromOperator "+" = return Plus
fromOperator "*" = return Times
fromOperator text = Left [i|${text} is not a valid operator|]

-- | Either an integer or a variable.
fromAtom :: Text -> Either Error Exp
fromAtom text =
  case signed decimal text of
    Right (n, "") ->
      return (N n)
    Right (_, _) ->
      Left [i|extra garbage after numeric in ${text}|]
    Left _ ->
      return (V (Text.unpack text))

fromExp :: Exp -> WellFormedSExpr Text
fromExp (N n) = WFSAtom (Text.pack (show n))
fromExp (V x) = WFSAtom (Text.pack x)
fromExp (Plus e1 e2) = WFSList [WFSAtom "+", fromExp e1, fromExp e2]
fromExp (Times e1 e2) = WFSList [WFSAtom "*", fromExp e1, fromExp e2]

prettyPrint :: Exp -> Text
prettyPrint =
  S.encodeOne (S.setFromCarrier fromWellFormed (S.basicPrint id))
  . fromExp
