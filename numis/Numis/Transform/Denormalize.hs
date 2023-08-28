module Numis.Transform.Denormalize where

import Numis.Payment
import GHC.Natural (Natural)

data Sign a = Positive a | Negative a
  deriving (Eq, Ord, Show, Functor)

denormalize :: Payment a (Settlement Natural) -> Payment (Scalar a) Natural
denormalize Payment{..} = case settlement of
  Assignment a -> Payment { payer = Asset payer, payee = Asset payee, settlement = a }
  Issuance a -> Payment { payer = Liability payer, payee = Asset payee, settlement = a }
  SetOff a -> Payment { payer = Asset payer, payee = Liability payee, settlement = a }
  Novation a -> Payment { payer = Liability payer, payee = Liability payee, settlement = a }

payerScalarToSign :: Scalar a -> Sign a
payerScalarToSign = \case
  Asset a -> Negative a
  Liability a -> Positive a

payeeScalarToSign :: Scalar a -> Sign a
payeeScalarToSign = \case
  Asset a -> Positive a
  Liability a -> Negative a
