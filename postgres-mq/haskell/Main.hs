{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

main = do
  conn <- connectPostgreSQL "postgres://mq@localhost/mq"
  execute conn "insert into public.queue (data) values (?)" $ Only ("1"::String)
  putStrLn "Done"
