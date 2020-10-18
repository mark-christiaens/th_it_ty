module Main where

from :: (c -> (a,b)) -> (c -> a, c -> b)
from f = (fst . f, snd . f)

to :: (c -> a, c -> b) -> (c -> (a,b))
to f c = (fst f c, snd f c)

-- An actual function with the signature c -> (a,b) cannot exist 
-- unless c == a and c == b?

-- For the Curry-Howard isomorphism.  Is it important that the 
-- functions are unconstrainted polymorphic?  Or put differently, 
-- would it suffice to show the existence of a specific function 
-- to prove logical conclusions ?
