{-# LANGUAGE QuasiQuotes #-}
module Numis.Language.Printer where

import Data.Text (Text)
import Numis.Payment 
import Data.String.Interpolate (i)
import Numis.Language.Expr
import Data.Foldable
import Data.List (intersperse)

printScalar :: Show a => Scalar a -> Text
printScalar (Asset x) = [i|has #{show x}|]
printScalar (Liability x) = [i|owes #{show x}|]

printSettlement :: Show a => Settlement a -> Text
printSettlement (Assignment x) = [i|assigns #{show x}|]
printSettlement (Issuance x) = [i|issues #{show x}|]
printSettlement (SetOff x) = [i|sets-off #{show x}|]
printSettlement (Novation x) = [i|novates #{show x}|]

printTitle :: Maybe Text -> Text 
printTitle = maybe "" (\t -> "as \"" <> t <> "\"")

printFact :: Fact' -> Text
printFact (ident, Fact{..}) = [i|#{ident} #{printScalar factScalar} #{printTitle factTitle}|]

printPayment :: Payment' -> Text
printPayment Payment{ ..} = 
  let Statement{..} = settlement
      settlementText = printSettlement statementSettlement
      titleText =  printTitle statementTitle
  in [i|#{payer} #{settlementText} to #{payee} #{titleText}|]

printLedger :: Ledger -> Text
printLedger = fold . intersperse "\n" . fmap (either printFact printPayment)
