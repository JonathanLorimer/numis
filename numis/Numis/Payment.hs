module Numis.Payment where

import Data.Bifunctor

data Payment a b =
  Payment
    { payer :: a
    , payee :: a
    , settlement :: b
    } deriving (Eq, Ord, Show, Functor)

inverse :: Payment a b -> Payment a b
inverse Payment { payer, payee, settlement } = Payment { payer = payee, payee = payer, settlement }

symmetricInverse :: a -> a -> b -> (Payment a b, Payment a b)
symmetricInverse a a' b = 
  let x = Payment a a' b
      y = inverse x
  in (x, y)

data Scalar a = Asset a | Liability a
  deriving (Eq, Ord, Show, Functor)

fromScalar :: Scalar a -> a
fromScalar = \case
  Asset a -> a
  Liability a -> a

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
