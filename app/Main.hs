{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib
import Env (initializeAppState,AppState,manager,port)

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
import qualified Network.HTTP.Client as HttpClient
import Control.Monad.Reader (runReaderT,asks,ask,ReaderT, runReader, Reader)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    state <- initializeAppState
    runReaderT app state

app :: ReaderT AppState IO ()
app = do
    port <- asks port
    liftIO $ putStrLn $ "running in " ++ show port 
    env <- ask
    liftIO $ run port $ runReader server $ env

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

server :: Reader AppState (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
server= server' <$> handle
    where server' handleWithEnv req res = case getHeader "proxy_to" (requestHeaders req) of 
            Just reqUrl -> do
                reqBody <- lazyRequestBody req
                (resHeaders,resBody) <- handleWithEnv req reqUrl reqBody
                res $ responseLBS status200 (addCorsHeaders (requestHeaderHost req) resHeaders) resBody
            Nothing -> res $ responseLBS status404 [] "Not Found"
    


type HandleRequest = Request -> B.ByteString -> BL.ByteString -> IO (ResponseHeaders,BL.ByteString)
handle :: Reader AppState HandleRequest
handle= handle' <$> asks manager
    where handle' manager req reqUrl reqBody = let 
            reqHeaders = requestHeaders req
            reqMethod = requestMethod req in
                proxyToURL manager (unpack reqUrl) reqMethod reqHeaders (toStrict reqBody)

addCorsHeaders:: Maybe B.ByteString->[Header]->[Header]
addCorsHeaders host headers = join [headers, catMaybes [
    Just (CI.mk "access-control-allow-methods","get,post,delete,put,delete"),
    Just (CI.mk "access-control-allow-credentials","true"),
    Just (CI.mk "access-control-allow-headers","Content-Type, user_name"),
    liftM ((,) (CI.mk "access-control-allow-origin")) host ]]

getQuery :: B.ByteString -> Query -> Maybe B.ByteString
getQuery name query = join $ List.lookup name query

getHeader :: B.ByteString -> [Header] -> Maybe B.ByteString
getHeader name headers = List.lookup (CI.mk name) headers