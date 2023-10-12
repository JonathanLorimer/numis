module Numis.Transform.Table where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numis.Language.Expr (Ledger, Fact (..), Payment', Statement (..), Fact')
import qualified Data.Set as Set
import Numis.Transform.Entities (agents)
import Numis.Payment (Scalar(..), fromScalar, Payment (..))
import Data.Vector.Generic.Mutable (write)
import Data.Functor ((<&>))
import Numis.Transform.Denormalize (denormalize, Sign (..), payerScalarToSign, payeeScalarToSign)

data Table = 
  Table 
    { headers :: Vector Text
    , rows :: [Vector Cell]
    }

data Cell = Empty | Item (Sign Natural) | Description Text

toTable :: Ledger -> Table
toTable ledger = Table{..}
  where
    headers' :: Vector (Scalar Text)
    headers' = Set.foldr (\a v -> V.cons (Asset a) $ V.cons (Liability a) v) V.empty (agents ledger)
  
    headers :: Vector Text
    headers = (headers' <&> \case
      Asset t -> t <> " Assets"
      Liability t -> t <> " Liabilities") <> (V.fromList ["Description"])

    emptyRow :: Vector Cell
    emptyRow = V.replicate (length headers) Empty

    lastIndex :: Int
    lastIndex = length headers - 1

    rows :: [Vector Cell]
    rows = foldr (\ledgerItem rows -> maybe rows (: rows) (mkRow ledgerItem)) [] ledger

    mkRow :: Either Fact' Payment' -> Maybe (Vector Cell)
    mkRow = \case
      (Left (ident, Fact{..})) ->
        let amnt = Item . Positive $ fromScalar factScalar
            col = const ident <$> factScalar 
            mIdx = V.elemIndex col headers'
            title = maybe Empty Description $ factTitle
        in mIdx <&> \idx -> 
            (`V.modify` emptyRow) \v -> do 
              write v idx amnt 
              write v lastIndex title 
      (Right p) ->
        let Payment{payer, payee, settlement = settlement'} = denormalize $ fmap statementSettlement p
            title = maybe Empty Description $ statementTitle (settlement p)
            mIdxs = liftA2 (,) (V.elemIndex payer headers') (V.elemIndex payee headers')
            payerAmount = const settlement' <$> payerScalarToSign payer
            payeeAmount = const settlement' <$> payeeScalarToSign payee
        in mIdxs <&> \(payerIdx, payeeIdx) -> 
            (`V.modify` emptyRow) \v -> do 
              write v payerIdx (Item payerAmount)
              write v payeeIdx (Item payeeAmount)
              write v lastIndex title
