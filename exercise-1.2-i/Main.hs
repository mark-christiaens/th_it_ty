module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

type BigType = Either Bool (Bool, Maybe Bool) -> Bool

-- f_example :: BigType
-- f_example 