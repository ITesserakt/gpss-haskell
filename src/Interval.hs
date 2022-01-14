{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Interval where

data Bound a
  = Unbounded
  | Included a
  | Excluded a deriving (Functor, Show)

data Infinity = Infinity

data Interval a = (Enum a, Ord a) => I (Bound a) (Bound a) | Empty

instance Eq a => Eq (Bound a) where
  Unbounded == Unbounded = True
  Included a == Included b = a == b
  Excluded a == Excluded b = a == b
  _ == _ = False

instance Ord a => Ord (Bound a) where
  compare Unbounded _ = GT
  compare _ Unbounded = LT
  compare (Included a) (Included b) = compare a b
  compare (Excluded a) (Excluded b) = compare a b
  compare (Included a) (Excluded b)
    | a == b = GT
    | otherwise = compare a b
  compare (Excluded a) (Included b)
    | a == b = LT
    | otherwise = compare a b

start ... end = I (Excluded start) (Excluded end)

start =...= end = I (Included start) (Included end)

start ...= end = I (Excluded start) (Included end)

start =... end = I (Included start) (Excluded end)

(<...>) :: (Enum a, Ord a) => Interval a
(<...>) = I Unbounded Unbounded

Infinity <... end = I Unbounded (Excluded end)

Infinity <...= end = I Unbounded (Included end)

start ...> Infinity = I (Excluded start) Unbounded

start =...> Infinity = I (Included start) Unbounded

(>...<) = Empty

instance Ord a => Semigroup (Interval a) where
  I Unbounded Unbounded <> _ = (<...>)
  _ <> I Unbounded Unbounded = (<...>)
  I s1 e1 <> I s2 e2 = I (max s1 s2) (max e1 e2)
  Empty <> i = i
  i <> Empty = i

instance Ord a => Monoid (Interval a) where
  mempty = Empty

instance Show a => Show (Interval a) where
  show Empty = "()"
  show (I Unbounded Unbounded) = "(...)"
  show (I (Included a) Unbounded) = "[" ++ show a ++ "; ...)"
  show (I (Excluded a) Unbounded) = "(" ++ show a ++ "; ...)"
  show (I Unbounded (Included a)) = "(...; " ++ show a ++ "]"
  show (I Unbounded (Excluded a)) = "(...; " ++ show a ++ ")"
  show (I (Included a) (Included b)) = "[" ++ show a ++ "; " ++ show b ++ "]"
  show (I (Excluded a) (Excluded b)) = "(" ++ show a ++ "; " ++ show b ++ ")"
  show (I (Included a) (Excluded b)) = "[" ++ show a ++ "; " ++ show b ++ ")"
  show (I (Excluded a) (Included b)) = "(" ++ show a ++ "; " ++ show b ++ "]"

instance Foldable Interval where
  foldMap _ Empty = mempty
  foldMap f (I Unbounded Unbounded) = foldMap f (I Unbounded Unbounded)
  foldMap f (I (Included a) (Included b))
   | a > b = mempty
   | a == b = f a
   | otherwise = f a <> foldMap f (a ... b) <> f b
  foldMap f (I (Included a) (Excluded b))
    | a >= b = mempty
    | otherwise = f a <> foldMap f (a ... b)
  foldMap f (I (Excluded a) (Included b))
    | a >= b = mempty
    |otherwise = foldMap f (a ... b) <> f b
  foldMap f (I (Excluded a) (Excluded b))
    | a >= b = mempty
    | otherwise = foldMap f (succ a =...= pred b)
  foldMap f (I Unbounded (Included a)) = foldMap f (Infinity <... a) <> f a
  foldMap f (I Unbounded (Excluded a)) = foldMap f (Infinity <...= pred a)
  foldMap f (I (Included a) Unbounded) = f a <> foldMap f (a ...> Infinity)
  foldMap f (I (Excluded a) Unbounded) = foldMap f (succ a =...> Infinity)
