{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Jmcgmqp.Worker
( cmdRun
) where

import Text.Printf (printf)
import System.Clock (getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL)
import Control.Concurrent (
  MVar, newEmptyMVar, putMVar, takeMVar, forkIO, threadDelay
  )
import Control.Concurrent.MVar (tryTakeMVar)
import Control.Monad.Loops (firstM)
import Control.Monad (void)
import Data.Maybe (fromMaybe)

insert :: Connection -> Int -> IO ()
insert conn i =
  void . execute conn "insert into public.queue (data) values (?)" . Only $ show i

worker :: Int -> MVar Bool -> MVar Int -> IO ()
worker _ quit result = do
  conn <- connectPostgreSQL "postgres://mq@localhost/mq"
  -- this is where the magic happens. Refactor later.
  let checkQuitAndInsert x = do
        q <- tryTakeMVar quit
        case q of
          Nothing -> do
              insert conn x
              return False
          Just _ -> return True
  x <- firstM checkQuitAndInsert [0..]
  putMVar result $ fromMaybe 0 x

forkWorker :: Int -> IO (MVar Bool, MVar Int)
forkWorker worker_id = do
  quit <- newEmptyMVar
  result <- newEmptyMVar
  void . forkIO $ worker worker_id quit result
  return (quit, result)

forkNWorkers :: Int -> IO ([MVar Bool], [MVar Int])
forkNWorkers n = do
  printf "Starting %d workers\n" n
  mvars <- mapM forkWorker [0..n]
  return $ unzip mvars

sample :: IO ([MVar Bool], [MVar Int]) -> IO Double
sample workers = do
  start <- getTime Monotonic
  (quits, results) <- workers
  threadDelay ((10::Int) ^ (6::Int)*3::Int)
  stop quits
  xs <- readResults results
  end <- getTime Monotonic
  mapM_ print xs
  let total = fromIntegral $ sum xs :: Double
  let nanosecs = fromIntegral $ toNanoSecs $ diffTimeSpec end start :: Double
  let secs = nanosecs * (10::Double) ^^ (-9::Int) :: Double
  -- printf "total: %.3f\n" total
  -- printf "ns: %.3f\n" nanosecs
  -- printf "s: %.3f\n" secs
  let ips = total / secs
  printf "Total: %d\n" $ sum xs
  printf "ips: %.3f\n" ips
  -- emulate slowdown with more than 2 workers for testing
  -- case length xs > 2 of
  -- True -> return 1
  -- _ -> return ips
  return ips

stop :: [MVar Bool] -> IO ()
stop = mapM_ (`putMVar` True)

readResults :: [MVar Int] -> IO [Int]
readResults = mapM takeMVar

cmdRun :: IO ()
cmdRun = do
  maxMVar <- newEmptyMVar
  putMVar maxMVar 0
  let checkQuitAndSample n_workers = do
      prev_max <- takeMVar maxMVar
      new <- sample $ forkNWorkers n_workers
      if new > prev_max
        then putMVar maxMVar new >> return False
        else putMVar maxMVar prev_max >> return True

  maybeMax <- firstM checkQuitAndSample $ map ((2::Int) ^) ([0..]::[Int])
  case maybeMax of
    Nothing -> putStrLn "Done"
    Just n_workers -> void $ firstM checkQuitAndSample [n_workers + 1..]
  putStrLn "Done"
