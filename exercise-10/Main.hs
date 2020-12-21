{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Data.Kind (Constraint, Type)

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Fst a b = Fst (a, b)

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a

newtype ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe as) = Just $ head as

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where -- surprising: needs UndecidableInstances
  eval (MapList f []) = []
  eval (MapList f (a : as)) = eval (f a) : eval (MapList f as)

type Exp a = a -> Type -- Any good explanation why it needs to be a function returning a `Type`?

type family EvalType (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance EvalType (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a

type instance EvalType (FromMaybe _1 ('Just a)) = a

type instance EvalType (FromMaybe a 'Nothing) = a

data ListToMaybeAtTypeLevel :: [a] -> Exp (Maybe a)

type instance EvalType (ListToMaybeAtTypeLevel '[]) = 'Nothing

type instance EvalType (ListToMaybeAtTypeLevel (a ': as)) = 'Just a

data MapListAtTypeLevel :: (a -> Exp b) -> [a] -> Exp [b]

type instance EvalType (MapListAtTypeLevel f '[]) = '[]

type instance EvalType (MapListAtTypeLevel f (a ': as)) = EvalType (f a) ': EvalType (MapListAtTypeLevel f as)

data FoldrAtTypeLevel :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance EvalType (FoldrAtTypeLevel f b '[]) = b

type instance EvalType (FoldrAtTypeLevel f b (a ': as)) = EvalType (f a b)

data Pure :: a -> Exp a

type instance EvalType (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b

type instance EvalType (k =<< e) = EvalType (k (EvalType e))

infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance EvalType ((f <=< g) x) = EvalType (f (EvalType (g x)))

infixr 1 <=<

data TyEq :: a -> b -> Exp Bool

type instance EvalType (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint

type instance EvalType (Collapse '[]) = (() :: Constraint)

type instance EvalType (Collapse (a ': as)) = (a, EvalType (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapListAtTypeLevel (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b

type instance EvalType (Pure1 f x) = f x

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance EvalType (Map f '[]) = '[]

type instance EvalType (Map f (a ': as)) = EvalType (f a) ': EvalType (Map f as)

type instance EvalType (Map f 'Nothing) = 'Nothing

type instance EvalType (Map f ('Just a)) = 'Just (EvalType (f a))

type instance EvalType (Map f ('Left x)) = 'Left x

type instance EvalType (Map f ('Right a)) = 'Right (EvalType (f a))

type instance EvalType (Map f (a, b)) = (a, EvalType f b)

data (++) :: [a] -> [a] -> Exp [a]
type instance EvalType ((++) '[] ys) = ys
type instance EvalType ((++) (x ': xs) ys) = x ': EvalType (xs ++ ys)

data Mappend :: a -> a -> Exp a
type instance EvalType (Mappend '() '()) = '()
type instance EvalType (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance EvalType (Mappend (a :: [k])(b :: [k])) = EvalType (a ++ b)

