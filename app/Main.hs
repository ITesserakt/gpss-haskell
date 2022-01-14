module Main where

import Block
import Control.Monad.Free (foldFree)
import Environment
import Facility
import Interval
import Queue
import Time
import Transaction

data Environment = Env
  { transaction :: Transaction,
    queue1 :: Queue,
    queue2 :: Queue,
    office1 :: Facility,
    office2 :: Facility,
    ticks :: Time
  }

instance Ticks Environment where
  currentTicks = ticks

program :: Block ()
program = do
  let env = Env Transaction Queue Queue Facility Facility (Time 0)

  work env $ Time 3600

  t <- inject $ Time 0 =...= Time 23
  t <- test env (used . queue1) "mark2"
  t <- test env (used . queue2) "mark1"
  t <- test env (\env -> uncurry (<=) (qLength . queue1 $ env, qLength . queue2 $ env)) "mark2"

  (q, t) <- label "mark1" . enterQueue $ queue1 env
  (f, t) <- wait (office1 env)
  t <- leaveQueue (queue1 env)
  (f, t) <- serve (office1 env) $ Time 7 =...= Time 23
  reject t

  (q, t) <- label "mark2" . enterQueue $ queue2 env
  (f, t) <- wait (office2 env)
  t <- leaveQueue (queue2 env)
  (f, t) <- serve (office2 env) $ Time 5 =...= Time 23
  reject t

compiler :: Block' a -> IO a
compiler (Inject _ n) = do
  putStrLn "Injecting"
  return $ n Transaction
compiler (Label _ _ n) = do
  putStrLn "Labeling"
  return n
compiler (Reject _ n) = do
  putStrLn "Rejecting"
  return n
compiler (EnterQueue _ n) = do
  putStrLn "Entering"
  return $ n (Queue, Transaction)
compiler (Test _ _ _ n) = do
  putStrLn "Testing"
  return $ n Transaction
compiler (Wait _ n) = do
  putStrLn "Waiting"
  return $ n (Facility, Transaction)
compiler (LeaveQueue _ n) = do
  putStrLn "Leaving"
  return $ n Transaction
compiler (Serve _ _ n) = do
  putStrLn "Serving"
  return $ n (Facility, Transaction)
compiler (WorkWhile _ _ n) = do
  putStrLn "Working while"
  return n

main :: IO ()
main = foldFree compiler program
