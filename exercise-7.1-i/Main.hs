{-# LANGUAGE GADTs #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- 7.1-i No functions of type "forall a . a -> r" are not interesting.
-- You cannot do anything useful with the a you are receiving.  Not can
-- you return the a to the outside world

-- 7.1-ii

data HasShow where
    -- HasShow :: t -> HasShow
    HasShow :: Show t => t -> HasShow

-- When you remove the Show t constraint, the following instance doesn't
-- compile anymore

instance Show HasShow where
    show (HasShow s) = "HasShow " ++ show s

