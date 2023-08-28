module Extras.Tuple where

tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]
