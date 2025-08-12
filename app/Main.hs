module Main where

import qualified WorkoutTemplate (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  WorkoutTemplate.someFunc
