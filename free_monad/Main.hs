{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    execState,
    modify,
  )

main :: IO ()
main = putStrLn $ "main"

data Op next
  = -- | Push element to the stack
    Push Double !next
  | -- | Pop element and return it, if it exists
    Pop !(Maybe Double -> next)
  | -- | Flip top two element of stack, if they exist
    Flip !next
  | -- | Add top two elements, if they exist
    Add !next
  | Subtract !next
  | Multiply !next
  | Divide !next
  | Split [next]
  | -- | Terminate program
    End
  deriving (Functor)

type Program = Free Op

push :: Double -> Program ()
-- push x = liftF $ Push x ()
-- push x = wrap . fmap return $ Push x ()
-- push x = wrap $ Push x $ return ()
-- push x = Free $ Push x $ Pure ()
push x = Free (Push x (Pure ()))

pop :: Program (Maybe Double)
-- pop = liftF $ Pop id
-- pop = Free . fmap return $ Pop id
-- pop = Free (Pop (fmap return id))
-- pop = Free (Pop (return . id))
pop = Free (Pop (Pure . id))

flip :: Program ()
-- flip = liftF $ Flip ()
-- flip = Free . fmap return $ Flip ()
-- flip = Free . fmap return $ Flip ()
flip = Free (Flip (Pure ()))

add :: Program ()
-- add = liftF $ Add ()
add = Free (Add (Pure ()))

subtract :: Program ()
-- subtract = liftF $ Subtract ()
subtract = Free (Subtract (Pure ()))

multiply :: Program ()
-- multiply = liftF $ Multiply ()
multiply = Free (Multiply (Pure ()))

divide :: Program ()
-- divide = liftF $ Divide ()
divide = Free (Divide (Pure ()))

split :: Program ()
split = Free (Split [Pure ()])

end :: forall a. Program a
-- end = liftF End
end = Free End

-- Compute 2 * 2 + 3 * 3, leaving result on top of stack
prog :: forall a. Program a
prog = do
  push 2
  push 2
  multiply
  push 3
  push 3
  multiply
  add
  end

prog2 :: forall a. Integer -> Program a
prog2 count = do
  mapM_ (\i -> push $ fromIntegral i) [1 .. count]
  mapM_ (\_ -> add) [1 .. (count - 1)]
  end

-- Double the top element of the stack.
double :: Program ()
double = do
  x <- pop
  case x of
    Nothing ->
      return ()
    Just x' ->
      push $ x' * 2

modStack :: (a -> a -> a) -> [a] -> [a]
modStack f (x : x' : xs) =
  f x x' : xs
modStack _ xs =
  xs

-- The existential type of the parameter forces the program to be
-- terminated with 'end'.
interpret :: (forall a. Program a) -> [Double]
interpret prog =
  execState (interpret' prog) []
  where
    interpret' :: Program a -> State [Double] ()
    interpret' (Free (Push x next)) = do
      modify (x :)
      interpret' next
    interpret' (Free (Pop next)) = do
      stack <- get
      case stack of
        x : xs -> do
          put $ xs
          interpret' $ next (Just x)
        [] -> interpret' $ next Nothing
    interpret' (Free (Flip next)) = do
      stack <- get
      case stack of
        x : x' : xs -> do
          put $ x' : x : xs
          interpret' next
        [] -> return ()
    interpret' (Free (Add next)) = do
      modify $ modStack (+)
      interpret' next
    interpret' (Free (Subtract next)) = do
      modify $ modStack (-)
      interpret' next
    interpret' (Free (Multiply next)) = do
      modify $ modStack (*)
      interpret' next
    interpret' (Free (Divide next)) = do
      modify $ modStack (/)
      interpret' next
    interpret' (Free (Split next)) = undefined
    interpret' (Free End) =
      return ()
