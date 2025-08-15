{-# LANGUAGE OverloadedStrings #-}
module WorkoutTemplateSpec (spec) where

import Test.Hspec
import Data.Yaml (decodeFileEither, ParseException)
import qualified WorkoutTemplate

spec :: Spec
spec = describe "WorkoutTemplate YAML parsing" $ do
  it "parses a valid workout_template.yaml file" $ do
    e <- decodeFileEither "workout_template.yaml" :: IO (Either ParseException WorkoutTemplate.WorkoutTemplate)
    case e of
      Left _  -> expectationFailure "Failed to parse workout_template.yaml"
      Right _ -> pure ()
