module Main where

import Network.Wai.Handler.Warp (run)
import Api.Server (app)

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app
