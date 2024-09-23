{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Jmcgmqp.Worker
( worker
) where

import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL)
import Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar)
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
