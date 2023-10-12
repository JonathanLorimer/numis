{-# LANGUAGE QuasiQuotes #-}
module Language.ParserSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numis.Language.Parser
import Text.Megaparsec (runParser)
import Numis.Language.Printer
import Numis.Payment
import qualified Data.Text as T
import Numis.Language.Expr
import Data.Foldable
import Data.List (intersperse)
import Numeric.Natural
import Data.Text (Text)

spec :: Spec
spec =
  describe "roundtrips" $ do
    it "fact" $
      hedgehog $ do
        f <- forAll genFact
        let p = printFact f
        res <- case runParser fact "test:rountrips|fact" p of 
                Left e -> footnoteShow f >> footnote (T.unpack p) >> footnoteShow e >> failure
                Right r -> pure r
        res === f
    it "settlement" $
      hedgehog $ do
        p <- forAll genPayment
        let t = printPayment p
        res <- case runParser payment "test:rountrips|settlement" t of 
                Left e -> footnoteShow p >> footnote (T.unpack t) >> footnoteShow e >> failure
                Right r -> pure r
        res === p
    it "ledger" $
      hedgehog $ do
        l <- forAll genLedger
        let p = printLedger l
        res <- case runParser ledger "test:rountrips|ledger" p of 
                Left e -> footnoteShow l >> footnote (T.unpack p) >> footnoteShow e >> failure
                Right r -> pure r
        res === l

genScalar :: Gen (a -> Scalar a)
genScalar = Gen.choice [pure Asset, pure Liability]

genSettlement :: Gen (a -> Settlement a)
genSettlement = Gen.choice [pure Assignment, pure SetOff, pure Novation, pure Issuance]

genTitle :: Gen (Maybe Text)
genTitle = Gen.maybe $ fold . intersperse " " <$> 
  Gen.list (Range.linear 1 10) (Gen.text (Range.linear 1 10) Gen.alphaNum)

genFact :: Gen Fact'
genFact = do 
  ident <- Gen.text (Range.linear 1 100) Gen.alphaNum
  factScalar <- genScalar <*> Gen.integral (Range.linear 0 1_000_000)
  factTitle <- genTitle
  pure (ident, Fact{..})

genPayment :: Gen Payment'
genPayment = do
  payer <- Gen.text (Range.linear 1 100) Gen.alphaNum
  payee <- Gen.text (Range.linear 1 100) Gen.alphaNum
  stlmnt <- genSettlement @Natural
  statementTitle <- genTitle
  statementSettlement <- stlmnt <$> (Gen.integral $ Range.linear 0 1_000_000)
  let settlement = Statement{..}
  pure Payment{ ..}

genLedger :: Gen Ledger
genLedger = Gen.list (Range.linear 0 100) $ Gen.choice [Left <$> genFact, Right <$> genPayment]
