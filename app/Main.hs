module Main where

import Network.Wai.Handler.Warp (run)
import Api.Server (app)
import System.Process (callProcess)


-- Ensure Elm frontend is built before starting the server.
-- Always invoke build script so updates are picked up; it's fast if unchanged.
ensureElm :: IO ()
ensureElm = do
  putStrLn "Ensuring Elm frontend is built (scripts/build-elm.sh)"
  callProcess "bash" ["scripts/build-elm.sh"]

main :: IO ()
main = do
  ensureElm
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app
