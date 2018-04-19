{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import qualified Data.List as List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM, join)
import Data.ByteString.Char8 (unpack)
import qualified Data.CaseInsensitive as CI
import Data.Maybe

main :: IO ()
main = run 3030 app

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req res = case getHeader "proxy_to" (requestHeaders req) of 
    Just reqUrl -> do
        reqBody <- lazyRequestBody req
        (resHeaders,resBody) <- handle req reqUrl reqBody
        res $ responseLBS status200 (addCorsHeaders (requestHeaderHost req) resHeaders) resBody
    Nothing -> res $ responseLBS status404 [] "Not Found"

handle :: Request -> B.ByteString -> BL.ByteString -> IO (ResponseHeaders,BL.ByteString)
handle req reqUrl reqBody = proxyToURL (unpack reqUrl) reqMethod reqHeaders (toStrict reqBody)
    where 
        reqHeaders = requestHeaders req
        reqMethod = requestMethod req

addCorsHeaders:: Maybe B.ByteString->[Header]->[Header]
addCorsHeaders host headers = join [headers, catMaybes [
    Just (CI.mk "access-control-allow-methods","get,post,delete,put,delete"),
    Just (CI.mk "access-control-allow-credentials","true"),
    Just (CI.mk "access-control-allow-headers","Content-Type, user_name"),
    liftM ((,) (CI.mk "access-control-allow-origin")) host ]]

getQuery :: B.ByteString -> Query -> Maybe B.ByteString
getQuery name = join . (liftM snd) . (List.find (\(key,value)->key==name))

getHeader :: B.ByteString -> [Header] -> Maybe B.ByteString
getHeader name = (liftM snd) . (List.find (\(key,value)->key==CI.mk name))