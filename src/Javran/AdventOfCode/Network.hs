{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Network
  ( fetchInputData
  , submitAnswer
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Types
import Text.HTML.DOM
import Text.XML.Cursor

fetchInputData :: Manager -> BS.ByteString -> Int -> Int -> IO BSL.ByteString
fetchInputData mgr mySession year day = do
  let url = intercalate "/" ["https://adventofcode.com", show year, "day", show day, "input"]
  reqPre <- parseRequest url
  let req = reqPre {requestHeaders = [("cookie", "session=" <> mySession)]}
  resp <- httpLbs req mgr
  guard $ responseStatus resp == ok200
  pure (responseBody resp)

submitAnswer
  :: Manager
  -> BS.ByteString
  -> Int
  -> Int
  -> Int
  -> BS.ByteString
  -> IO [T.Text]
submitAnswer mgr mySession year day level answer = do
  let url = intercalate "/" ["https://adventofcode.com", show year, "day", show day, "answer"]
  reqPre <- parseRequest ("POST " <> url)
  let req =
        urlEncodedBody
          [("level", encodeUtf8 $ T.pack (show level)), ("answer", answer)]
          $ reqPre {requestHeaders = [("cookie", "session=" <> mySession)]}
  resp <- httpLbs req mgr
  let parsed = fromDocument $ parseLBS $ responseBody resp
      xs = parsed $// element "main" >=> child >=> element "article" >=> descendant
  pure $ concatMap content xs
