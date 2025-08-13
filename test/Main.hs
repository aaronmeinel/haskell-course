module Main where

import Test.Hspec
import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isRight)
import WorkoutTemplate (WorkoutTemplate)



main :: IO ()
main = hspec $ do
     describe "WorkoutTemplate YAML parsing" $ do
          it "parses a valid workout_template.yaml file" $ do
            result <- decodeFileEither "workout_template.yaml" :: IO (Either Data.Yaml.ParseException WorkoutTemplate)
            result `shouldSatisfy` isRight
            -- You can add more detailed checks once FromJSON is implemented

