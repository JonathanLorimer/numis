module Numis.Obligation where

import Numis.Relation

data Scalar a = Asset a | Liability a
  deriving (Eq, Ord, Show)

asset :: a -> b -> Payment a (Scalar b)
asset a = Payment a a . Asset

liability :: a -> b -> Payment a (Scalar b)
liability a = Payment a a . Liability

data Settlement a = Assignment a | Issuance a | SetOff a | Novation a
  deriving (Eq, Ord, Show)

assignment :: a -> a -> b -> Payment a (Settlement b)
assignment a a' = Payment a a' . Assignment

issuance :: a -> a -> b -> Payment a (Settlement b)
issuance a a' = Payment a a' . Issuance

setOff :: a -> a -> b -> Payment a (Settlement b)
setOff a a' = Payment a a' . SetOff

novation :: a -> a -> b -> Payment a (Settlement b)
novation a a' = Payment a a' . Novation


