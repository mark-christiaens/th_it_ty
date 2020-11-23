{-# LANGUAGE GeneralisedNewtypeDeriving, DeriveFunctor #-}

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype T1 a = T1 (Int -> a)
    -- deriving Functor
newtype T2 a = T2 (a -> Int)
    -- deriving Functor
newtype T3 a = T3 (a -> a)
    -- deriving Functor
newtype T4 a = T4 ((Int -> a) -> Int)
    -- deriving Functor
newtype T5 a = T5 ((a -> Int) -> Int)
    -- deriving Functor

instance Functor T1 where
    fmap f (T1 ff) = T1 (f . ff)

-- instance Functor T2 where
--     fmap f (T2 ff) = undefined

-- instance Functor T3 where
--     fmap f (T3 ff) = undefined

-- instance Functor T4 where
--     fmap f (T4 ff) = undefined

instance Functor T5 where
    fmap f (T5 ff) = T5 (\fff -> ff(fff.f))

-- fmap id (T5 ff) = T5 (\fff -> ff(fff.id)) = T5 (\fff -> ff(fff)) = T5 (ff)

-- fmap (f.g) (T5 ff) = T5 (\fff -> ff(fff.(f.g))) = T5 (\fff -> ff(fff.(f.g)))
-- fmap f (fmap g (T5 ff)) = fmap f (T5 (\fff -> ff(fff.g))) = fmap f (T5 (\ffff -> ff(ffff.g))) = 
-- T5 (\fff -> (\ffff -> ff(ffff.g))(fff.f)) = T5 (\fff -> (ff((fff.f).g))) = T5 (\fff -> ff(fff.(f.g)))