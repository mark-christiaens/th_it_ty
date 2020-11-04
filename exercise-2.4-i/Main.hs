{-# LANGUAGE TypeFamilies, DataKinds #-}

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Bool = True | False

type family Or (x :: Bool) (y :: Bool) :: Bool where
Or 'True y = 'True
Or 'False y = y