{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Maybe
import Database.PostgreSQL.Simple

insert :: Connection -> Int -> IO Int
insert conn i = do
  execute conn "insert into public.queue (data) values (?)" $ Only $ show i
  return i

worker :: MVar Bool -> MVar Int -> IO ()
worker quit result = do
  conn <- connectPostgreSQL "postgres://mq@localhost/mq"
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

main = do
  quit <- newEmptyMVar
  result <- newEmptyMVar :: IO (MVar Int)
  forkIO $ worker quit result
  threadDelay (10^6::Int)
  putMVar quit True
  n <- takeMVar result
  putStrLn $ show (n::Int)
  putStrLn "Done"
