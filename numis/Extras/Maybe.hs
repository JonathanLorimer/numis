module Extras.Maybe where

-- | Takes a maybe and an action to run on the `Nothing` case.
-- In the `Just` case returns the value via pure
handleNothing :: (Applicative f) => Maybe a -> f a -> f a
handleNothing m action = maybe action pure m
