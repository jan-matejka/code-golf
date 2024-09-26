{-# LANGUAGE OverloadedStrings, FlexibleContexts, OverloadedRecordDot #-}
module Jmcgmqp.Worker
( cmdRun
) where

import Data.Kind (Type)
import Text.Printf (printf)
import System.Clock (getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec, TimeSpec)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL)
import Control.Concurrent (
  MVar, newEmptyMVar, putMVar, takeMVar, forkIO, threadDelay
  )
import Control.Concurrent.Extra (newVar, Var, readVar, writeVar)
import Control.Monad.Loops (firstM)
import Control.Monad (void, forM_)
import Control.Monad.IfElse (returning)
import Data.Maybe (fromJust)

import Jmcgmqp.Runtime (Instance(Instance), config)
import Jmcgmqp.Config (duration, Config(Config))

insert :: Connection -> Int -> IO ()
insert conn =
  void . execute conn "insert into public.queue (data) values (?)" . Only . show

-- | Perform monadic `fs` until p is True,
-- then return the result of last executed `f` from `fs`
-- or `d` if there is no result yet.
--
-- A weird combination of whileM and iterateM from Conrol.Monad.Loops
until'1 :: Monad m => Maybe a -> m Bool -> [m a] -> m (Maybe a)
until'1 d _ [] = return d
until'1 d p (f:fs) = do
  x <- p
  if x then return d
  else do
    y <- f
    until'1 (Just y) p fs

type WorkerResult :: Type
data WorkerResult = WorkerResult {
  workerId :: Int,
  messagesTotal :: Int,
  duration :: TimeSpec,
  messagesPerSecond :: Double
} deriving stock (Show)

newWorkerResult :: Int -> Int -> TimeSpec -> WorkerResult
newWorkerResult wId mTotal dur = WorkerResult {
  workerId = wId,
  messagesTotal = mTotal,
  duration = dur,
  messagesPerSecond = mps mTotal dur
  }

mps :: Int -> TimeSpec -> Double
mps n d = fromIntegral n / secs :: Double
  where
    nsecs :: Double
    nsecs = fromIntegral $ toNanoSecs d
    secs :: Double
    secs = nsecs * (10::Double) ^^ (-9::Int)

type Results :: Type
data Results = Results {
  workers :: [WorkerResult],
  messagesTotal :: Int,
  duration :: TimeSpec,
  messagesPerSecond :: Double
} deriving stock (Show)

newResults :: [WorkerResult] -> Results
newResults ws = Results {
  workers = ws,
  messagesTotal = totalMsg,
  duration = totalDur,
  messagesPerSecond = mps totalMsg totalDur
  }
  where
    totalMsg :: Int
    totalMsg = sum $ map (\WorkerResult{messagesTotal=n} -> n) ws
    totalDur :: TimeSpec
    totalDur = sum $ map (\WorkerResult{duration=d} -> d) ws

printResults :: Results -> IO ()
printResults rs = do
  mapM_ printWorker rs.workers
  printf "Total: %d\n" rs.messagesTotal
  printf "Total mps: %.3f\n" rs.messagesPerSecond
  where
    printWorker :: WorkerResult -> IO ()
    printWorker w = printf "%d: %d\n" w.workerId w.messagesTotal

worker :: Int -> QuitVar -> MVar WorkerResult -> IO ()
worker wId quit result = do
  conn <- connectPostgreSQL "postgres://mq@localhost/mq"
  start <- getTime Monotonic
  x <- until'1 Nothing (readVar quit) [returning (insert conn) i | i <- [0..]]
  end <- getTime Monotonic
  putMVar result $ newWorkerResult wId (fromJust x) (diffTimeSpec end start)

forkWorker :: QuitVar -> Int -> IO (MVar WorkerResult)
forkWorker quit worker_id = do
  result <- newEmptyMVar
  void . forkIO $ worker worker_id quit result
  return result

-- | Signal to workers they should quit.
type QuitVar :: Type
type QuitVar = Var Bool

-- | Sleep for Config.duration seconds and print countdown after every
-- second waited
waitDuration :: Instance -> IO ()
waitDuration Instance{config=Config{duration=n}} = forM_ [n,n-1..1] sleep
  where
    sleep :: Int -> IO ()
    sleep x = print x >> threadDelay 1_000_000

sample :: Instance -> Int -> IO Double
sample app n_workers = do
  q <- newVar False

  printf "Starting %d workers\n" n_workers
  results <- mapM (forkWorker q) [0..n_workers]

  putStrLn "Waiting"
  waitDuration app

  writeVar q True
  rs <- newResults <$> mapM takeMVar results
  printResults rs

  return rs.messagesPerSecond

cmdRun :: Instance -> IO ()
cmdRun app = do
  maxMVar <- newEmptyMVar
  putMVar maxMVar 0
  let checkQuitAndSample n_workers = do
      prev_max <- takeMVar maxMVar
      new <- sample app n_workers
      if new > prev_max
        then putMVar maxMVar new >> return False
        else putMVar maxMVar prev_max >> return True

  maybeMax <- firstM checkQuitAndSample $ map ((2::Int) ^) ([0..]::[Int])
  case maybeMax of
    Nothing -> putStrLn "Done"
    Just n_workers -> void $ firstM checkQuitAndSample [n_workers + 1..]
  putStrLn "Done"
