{-# LANGUAGE LambdaCase #-}

module Main where

from :: ((b -> a), (c -> a)) -> (Either b c -> a)
from (fb, fc) = \case
  Left b -> fb b
  Right c -> fc c

to :: (Either b c -> a) -> ((b -> a), (c -> a))
to fe = (fe . Left, fe . Right)
