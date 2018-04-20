module Env where

import qualified Network.HTTP.Client as HttpClient
import Control.Monad.Reader (runReader, Reader)

data AppState = AppState {
    manager :: HttpClient.Manager,
    port :: Int
}

initializeAppState :: IO AppState
initializeAppState = do
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    return AppState {
        manager = manager,
        port = 3030
    }