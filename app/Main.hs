module Main where

import Api
import Data.Maybe
import System.Environment
import Text.Read

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  connectionString <- getEnv "DB_CONNECTION_STRING"
  let maybePort = portEnv >>= \x -> readMaybe x :: Maybe Int
  startApp (fromMaybe 8080 maybePort) connectionString
