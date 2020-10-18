module Main where

from :: (b -> c -> a) -> (b, c) -> a
from f = \(b, c) -> f b c

to :: ((b, c) -> a) -> (b -> c -> a)
to f = \b -> \c -> f (b, c)

-- To discuss: are all Haskell types, ADTs?  GADTs?
-- Do types with infinite cardinality exist (recursive types?)