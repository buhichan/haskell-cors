{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( proxyToURL,
    makeRequest
    ) where

import Network.HTTP.Client as HttpClient
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (Header)

makeRequest url method headers body = do
    initialRequest <- HttpClient.parseRequest url
    return $ initialRequest {
        HttpClient.method = parseMethod method,
        HttpClient.requestBody = HttpClient.RequestBodyBS body,
        HttpClient.requestHeaders = headers}

proxyToURL:: HttpClient.Manager -> String->B.ByteString->[Header]->B.ByteString->IO([Header], BL.ByteString)
proxyToURL manager url method headers body = do
    let req' = makeRequest url method headers body 
    (case req' of 
        Just req -> do
            res <- HttpClient.httpLbs req manager
            return (HttpClient.responseHeaders res,  HttpClient.responseBody res)
        Nothing -> return ([],"Invalid Url") )

parseMethod :: B.ByteString -> Method
parseMethod "POST" = "POST"
parseMethod "DELETE" = "DELETE"
parseMethod "PUT" = "PUT"
parseMethod "OPTION" = "OPTION"
parseMethod _ = "GET"