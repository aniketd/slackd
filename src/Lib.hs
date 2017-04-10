{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( send
    ) where

import Data.Aeson
import Data.Monoid ((<>))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header

slackHook = "https://hooks.slack.com/services/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"

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  request <- parseRequest url
  return $ request
    { method = "POST"
    , requestBody = body
    , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
    }

send :: RequestBody -> IO ()
send s = do
  manager <- newManager tlsManagerSettings
  request <- buildRequest slackHook s
  response <- httpLbs request manager
  -- let Just obj = decode (responseBody response)
  -- print (obj :: Object)
  print $ responseBody response
