module Numis.Relation (
  Payment(..),
  inverse,
  symmetricInverse
) where

import Data.Bifunctor

data Payment a b =
  Payment
    { payer :: a
    , payee :: a
    , relation :: b
    } deriving (Eq, Ord, Show, Functor)

inverse :: Payment a b -> Payment a b
inverse Payment { payer, payee, relation } = Payment { payer = payee, payee = payer, relation }

symmetricInverse :: a -> a -> b -> (Payment a b, Payment a b)
symmetricInverse a a' b = 
  let x = Payment a a' b
      y = inverse x
  in (x, y)

