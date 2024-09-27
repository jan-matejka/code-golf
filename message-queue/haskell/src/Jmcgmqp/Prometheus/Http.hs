module Jmcgmqp.Prometheus.Http
( pushAdd
) where

import Data.Map (Map, toList)
import Data.List (intercalate)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Network.URI.Encode (encode)
import Network.HTTP.Client (
  newManager, parseRequest, defaultManagerSettings, httpLbs, responseStatus,
  method, requestBody, RequestBody(RequestBodyLBS)
  )
import Network.HTTP.Types.Status (statusCode)

mkURI :: Map ByteString ByteString -> String
mkURI labels =
  "http://localhost:9091/metrics/job/mq-producer/" ++
    (intercalate "/" $ map item $ toList labels)
  where
    item :: (ByteString, ByteString) -> String
    item (k, v) = (encode $ unpack k) ++ "/" ++ (encode $ unpack v)

pushAdd :: Map ByteString ByteString -> ByteString -> IO ()
pushAdd labels body = do
  manager <- newManager defaultManagerSettings

  iReq <- parseRequest $ mkURI labels
  let req = iReq { method = "POST", requestBody = RequestBodyLBS body }
  resp <- httpLbs req manager
  if resp.responseStatus.statusCode /= 200 then error $ show resp
  else return ()
