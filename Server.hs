{-# LANGUAGE OverloadedStrings #-}
module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (statusOK)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 (unpack)
import qualified Data.Text.Lazy as T
import Control.Monad.Trans (liftIO)

anyApp :: (Request -> String) -> Application
anyApp app req = do
  liftIO $ putStrLn $ show $ pathInfo req
  return $ responseLBS status headers (L.pack $ app req)
    where
      status = statusOK
      headers = [("Content-Type", "text/html")]

runServer :: (Request -> String) -> IO ()
runServer app = do
  run 8080 $ anyApp app

