{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (join)
main :: IO ()
main = putStrLn "Hello, Haskell!"

-- Rank == 1
ex1 :: Int -> (forall a. a -> a)
ex1 _ = id

-- Rank == 2
ex2 :: (a -> b) -> (forall c. c -> a) -> b
ex2 f1 f2 = f1 $ f2 0

invoke2 :: [Char]
invoke2 = ex2 (const "world") (const "hello")

-- Rank == 3
-- ex3 :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
-- ex3 = undefined

newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f c = Cont $ \ff -> unCont c (ff . f)

instance Applicative Cont where
  pure a = Cont $ \f -> f a
  (<*>) c1 c2 = Cont $ \f -> f (unCont c1 id $ unCont c2 id)

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (>>=) c1 fm = fm $ unCont c1 id

newtype ContT m a = ContT {runContT :: forall r . (a -> m r) -> m r}

instance Functor (ContT m) where
  fmap :: (a -> b) -> ContT m a -> ContT m b
  fmap f ct = ContT $ \ff -> runContT ct (ff . f)

instance Monad m => Applicative (ContT m) where
    pure a = ContT $ \f -> f a
    (<*>) c1 c2 = ContT $ \f -> (run1 <*> run2) >>= f
        where 
            run1 = runContT c1 pure
            run2 = runContT c2 pure

instance Monad m => Monad (ContT m) where
    (>>=) :: ContT m a -> (a -> ContT m b) -> ContT m b
    (>>=) c1 fm = run6
        where
            run6 = ContT $ \f -> run5 >>= f
            run5 = join run4
            run4 = run3 <*> pure pure
            run3 = runContT <$> run2
            run2 = fm <$> run
            run = runContT c1 pure
            
