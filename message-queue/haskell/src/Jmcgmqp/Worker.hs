{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Jmcgmqp.Worker
( worker
) where

import Database.PostgreSQL.Simple
import Control.Concurrent.MVar
import Control.Monad.Loops

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
