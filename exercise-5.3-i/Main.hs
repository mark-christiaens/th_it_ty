{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Constraint, Type)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

-- instance Eq (HList '[]) where
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--     (a :# as) == (b :# bs) = a == b && as == bs

-- Note : the above cannot check HLists of different lengths

-- instance Ord (HList '[]) where
--     compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--     compare (a :# as) (b :# bs) = if cab == EQ then compare as bs else cab
--         where cab = compare a b

-- instance Show (HList '[]) where
--     show HNil = "HNil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--     show (t :# ts) = show t ++ ":#" ++ show ts

-- show True :# HNil

type family AllEq (ts :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts) 

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All _ '[] = () 
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    (a :# as) == (b :# bs) = a == b && as == bs    

instance (All Ord ts, All Eq ts) => Ord (HList ts) where
    compare (a :# as) (b :# bs) = if cab == EQ then compare as bs else cab
        where cab = compare a b

instance (All Show ts) => Show (HList ts) where
    show HNil = "HNil"
    show (t :# ts) = show t ++ ":#" ++ show ts
