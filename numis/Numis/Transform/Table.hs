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
import Data.List.NonEmpty (NonEmpty)
import Data.Bifunctor

type Matrix a = Vector (Vector a)

data Table = 
  Table 
    { headers :: Vector Text
    , rows :: Matrix (Maybe (Sign Natural))
    , descriptions :: Vector (Maybe Text)
    }

toTable :: Ledger -> Table
toTable ledger = Table{..}
  where
    headers :: Vector Text
    headers = V.fromList . Set.toList $ agents ledger

    emptyRow :: Vector (Maybe (Sign Natural))
    emptyRow = V.replicate (length headers * 2) Nothing

    rows :: Matrix (Maybe (Sign Natural))
    rows = V.fromList $ foldr (\ledgerItem rows -> maybe rows (: rows) (mkRow ledgerItem)) [] ledger

    descriptions :: Vector (Maybe Text)
    descriptions = foldr (\ledgerItem descs -> getDescription ledgerItem `V.cons` descs) V.empty ledger

    getDescription :: Either Fact' Payment' -> Maybe Text
    getDescription (Left (_, Fact{..})) = factTitle
    getDescription (Right p) = statementTitle $ settlement p

    mkRow :: Either Fact' Payment' -> Maybe (Vector (Maybe (Sign Natural)))
    mkRow t = case t of
      (Left (ident, Fact{..})) ->
        let amnt = Just . Positive $ fromScalar factScalar
            col = ident <$ factScalar 
            mIdx = mkIdx col <$> V.elemIndex (fromScalar col) headers
        in mIdx <&> \idx -> 
            (`V.modify` emptyRow) \v -> write v idx amnt 
      (Right p) ->
        let Payment{payer, payee, settlement = settlement'} = denormalize $ fmap statementSettlement p
            mIdxs =  
              liftA2 
                (,) 
                (mkIdx payer <$> V.elemIndex (fromScalar payer) headers) 
                (mkIdx payee <$> V.elemIndex (fromScalar payee) headers)
            payerAmount = settlement' <$ payerScalarToSign payer
            payeeAmount = settlement' <$ payeeScalarToSign payee
        in mIdxs <&> \(payerIdx, payeeIdx) -> 
            (`V.modify` emptyRow) \v -> do 
              write v payerIdx (Just payerAmount)
              write v payeeIdx (Just payeeAmount)

mkIdx :: Scalar a -> Int -> Int
mkIdx s col = 
  case s of 
    Asset _ -> col * 2
    Liability _ -> (col * 2) + 1
