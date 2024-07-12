{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import GHC.Num.Integer
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Maybe
import Database.PostgreSQL.Simple
import Text.Printf
import System.Clock

insert :: Connection -> Int -> IO Int
insert conn i = do
  execute conn "insert into public.queue (data) values (?)" $ Only $ show i
  return i

worker :: Int -> MVar Bool -> MVar Int -> IO ()
worker id quit result = do
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
  putMVar result $ x_or_zero x
  where
    x_or_zero Nothing = 0
    x_or_zero (Just x) = x


forkWorker :: Int -> IO (MVar Bool, MVar Int)
forkWorker id = do
  quit <- newEmptyMVar
  result <- newEmptyMVar
  forkIO $ worker id quit result
  return (quit, result)

n_workers :: Int -> IO ([MVar Bool], [MVar Int])
n_workers n = do
  printf "Starting %d workers\n" n
  mvars <- mapM forkWorker [0..n]
  return $ unzip mvars

sample :: IO ([MVar Bool], [MVar Int]) -> IO ()
sample workers = do
  start <- getTime Monotonic
  (quits, results) <- workers
  threadDelay (10^6*3::Int)
  stop quits
  xs <- read_results results
  end <- getTime Monotonic
  mapM_ (putStrLn.show) xs
  let total = fromIntegral $ sum xs :: Double
  let nanosecs = fromIntegral $ toNanoSecs $ diffTimeSpec end start :: Double
  let secs = nanosecs * 10 ^^ (-9) :: Double
  -- printf "total: %.3f\n" total
  -- printf "ns: %.3f\n" nanosecs
  -- printf "s: %.3f\n" secs
  let ips = total / secs
  printf "Total: %d\n" $ sum xs
  printf "ips: %.3f\n" ips

stop :: [MVar Bool] -> IO ()
stop = mapM_ (\mvar -> putMVar mvar True)

read_results :: [MVar Int] -> IO [Int]
read_results = mapM (\result -> takeMVar result)

main = do
  mapM_ (\x -> sample (n_workers x)) $ map (2^) [0..]
  putStrLn "Done"
