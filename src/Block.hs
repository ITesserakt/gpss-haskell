{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Block where

import Control.Monad.Free
import Environment
import Facility
import Queue
import Time
import Transaction

data Block' next where
  Inject :: TimeInterval -> (Transaction -> next) -> Block' next
  Label :: String -> Block a -> next -> Block' next
  Reject :: Transaction -> next -> Block' next
  EnterQueue :: Queue -> ((Queue, Transaction) -> next) -> Block' next
  Test :: a -> (a -> Bool) -> String -> (Transaction -> next) -> Block' next
  Wait :: Facility -> ((Facility, Transaction) -> next) -> Block' next
  LeaveQueue :: Queue -> (Transaction -> next) -> Block' next
  Serve :: Facility -> TimeInterval -> ((Facility, Transaction) -> next) -> Block' next
  WorkWhile :: a -> (a -> Bool) -> next -> Block' next

instance Functor Block' where
  fmap f (Inject t g) = Inject t $ f . g
  fmap f (Reject t n) = Reject t $ f n
  fmap f (Label s bl g) = Label s bl $ f g
  fmap f (EnterQueue q g) = EnterQueue q $ f . g
  fmap f (Test q m l g) = Test q m l $ f . g
  fmap f (Wait fac g) = Wait fac $ f . g
  fmap f (LeaveQueue q g) = LeaveQueue q $ f . g
  fmap f (Serve fac t g) = Serve fac t $ f . g
  fmap f (WorkWhile a m n) = WorkWhile a m $ f n

type Block = Free Block'

queue :: Block Queue
queue = return Queue

inject :: TimeInterval -> Block Transaction
inject interval = liftF $ Inject interval id

label :: String -> Block a -> Block a
label l bl = wrap $ Label l bl bl

reject :: Transaction -> Block ()
reject t = liftF $ Reject t ()

enterQueue :: Queue -> Block (Queue, Transaction)
enterQueue qId = liftF $ EnterQueue qId id

test :: a -> (a -> Bool) -> String -> Block Transaction
test q m l = liftF $ Test q m l id

wait :: Facility -> Block (Facility, Transaction)
wait f = liftF $ Wait f id

leaveQueue :: Queue -> Block Transaction
leaveQueue q = liftF $ LeaveQueue q id

serve :: Facility -> TimeInterval -> Block (Facility, Transaction)
serve f t = liftF $ Serve f t id

workWhile :: a -> (a -> Bool) -> Block ()
workWhile e m = liftF $ WorkWhile e m ()

work :: Ticks a => a -> Time -> Block ()
work e t = workWhile e ((>= t) . currentTicks)
