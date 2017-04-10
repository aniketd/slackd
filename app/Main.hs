{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Client (RequestBody(RequestBodyBS))
import Network.Wai.Handler.Warp (run)
import Data.Monoid ((<>))
import Data.ByteString.Char8 hiding (putStrLn)
import qualified Data.ByteString.Lazy as LBS
import Lib (send)

app :: Application
app req respond = do
    body <- strictRequestBody req
    send $ "{\"text\": \"```" <>
      "REMOTE:     " <> RequestBodyBS remote <> "\n" <>
      -- "USERAGENT:  " <> RequestBodyBS userAgent <> "\n" <>
      -- "PATH:       " <> RequestBodyBS path <> "\n" <>
      -- "QUERY:      " <> RequestBodyBS query <> "\n" <>
      -- "METHOD:     " <> RequestBodyBS method <> "\n" <>
      -- "HEAD:       " <> RequestBodyBS head <> "\n" <>
      "This is a clean test" <>
      "```\"}"
    print $ "REMOTE:     " <> remote
    print $ "USERAGENT:  " <> userAgent
    print $ "PATH:       " <> path
    print $ "QUERY:      " <> query
    print $ "METHOD:     " <> method
    print $ "HEAD:       " <> head
    print $ "BODY:       " <> body
    putStrLn "------------------------------------------------------------------"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"
    where remote = (pack $ show $ remoteHost req) :: ByteString
          userAgent = (pack $ show $ requestHeaderUserAgent req) :: ByteString
          path = rawPathInfo req :: ByteString
          query = rawQueryString req :: ByteString
          method = requestMethod req :: ByteString
          head = (pack $ show $ requestHeaders req) :: ByteString

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
