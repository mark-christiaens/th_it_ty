{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Data.Coerce (Coercible, coerce)
main :: IO ()
-- main = print makeCase4
main = undefined

-- Roles of Either a b
-- a : representational
-- b : representational

-- Roles Proxy a
-- a : representational

newtype Age = MkAge { unAge :: Int }

type family ToOtherType x
type instance ToOtherType Age = Int
type instance ToOtherType Int = Bool

data Testing a phant where
    -- Case1 :: a -> Testing a phant
    Case2 :: a -> phant -> Testing a phant -- "phant" is no longer a phantom type
    -- Case3 :: Show a => a -> Testing a phant -- This adds a constraint "Show a" to the representation of a "Case3".  You cannot convert that to a "Case1" that does not have constraint.  Seems to be that adding any Class constraint makes the compiler unhappy
    -- Case4 :: a -> Testing (ToOtherType a) phant

-- instance Show a => Show (Testing a phant) where
--     show (Case1 a) = show a
--     show (Case4 a) = show a


-- makeCase4 :: Testing Bool Double
-- makeCase4 = Case4 (5 :: Int)  
   

user :: (Show a1, Show a2, Coercible a1 a2) => Testing a1 p1 -> Testing a2 p2
user = coerce
    



