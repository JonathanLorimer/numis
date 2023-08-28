module Numis.Transform.Table where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numis.Language.Expr (Ledger, Fact, Payment')
import qualified Data.Set as Set
import Numis.Transform.Entities (agents)
import Numis.Payment (Scalar(..), fromScalar, Payment (..))
import Data.Vector.Generic.Mutable (write)
import Data.Functor ((<&>))
import Numis.Transform.Denormalize (denormalize, Sign (..), payerScalarToSign, payeeScalarToSign)

data Table = 
  Table 
    { headers :: Vector Text
    , rows :: [Vector (Maybe (Sign Natural))]
    }

toTable :: Ledger -> Table
toTable ledger = Table{..}
  where
    headers' :: Vector (Scalar Text)
    headers' = Set.foldr (\a v -> V.cons (Asset a) $ V.cons (Liability a) v) V.empty (agents ledger)
  
    headers :: Vector Text
    headers = headers' <&> \case
      Asset t -> t <> " Assets"
      Liability t -> t <> " Liabilities"

    emptyRow :: Vector (Maybe (Sign Natural))
    emptyRow = V.replicate (length headers') Nothing

    rows :: [Vector (Maybe (Sign Natural))]
    rows = foldr (\ledgerItem rows -> maybe rows (: rows) (mkRow ledgerItem)) [] ledger

    mkRow :: Either Fact Payment' -> Maybe (Vector (Maybe (Sign Natural)))
    mkRow = \case
      (Left (ident, scalar)) ->
        let amnt = Just . Positive $ fromScalar scalar
            col = const ident <$> scalar 
            mIdx = V.elemIndex col headers'
        in mIdx <&> \idx -> V.modify (\v -> write v idx amnt) emptyRow
      (Right p) ->
        let Payment{..} = denormalize p
            mIdxs = liftA2 (,) (V.elemIndex payer headers') (V.elemIndex payee headers')
            payerAmount = const settlement <$> payerScalarToSign payer
            payeeAmount = const settlement <$> payeeScalarToSign payee
        in mIdxs <&> \(payerIdx, payeeIdx) -> 
            (`V.modify` emptyRow) \v -> do 
              write v payerIdx (Just payerAmount)
              write v payeeIdx (Just payeeAmount)
