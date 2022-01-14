{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Environment where

import Data.Kind (Type)
import Time (Time)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 %:

(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance Show (HList '[]) where
  show HNil = "null"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs

class Ticks a where
  currentTicks :: a -> Time
