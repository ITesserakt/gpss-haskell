{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Time where
  
import Interval

newtype Time = Time Double deriving (Show, Fractional, Num, Ord, Eq, Enum)

type TimeInterval = Interval Time