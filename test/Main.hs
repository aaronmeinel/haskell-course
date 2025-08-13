module Main where

import Test.Hspec
import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isRight)
import WorkoutTemplate (WorkoutTemplate, MuscleGroup(..))
import Mesocycle (Mesocycle(..), MesocycleWeek(..), MesocycleWorkout(..), MesocycleExercise(..), PreExerciseFeedback(..), PostExerciseFeedback(..), Soreness(..), JointPain(..), Pump(..), Workload(..))


main :: IO ()
main = hspec $ do
     describe "WorkoutTemplate YAML parsing" $ do
          it "parses a valid workout_template.yaml file" $ do
            result <- decodeFileEither "workout_template.yaml" :: IO (Either Data.Yaml.ParseException WorkoutTemplate)
            result `shouldSatisfy` isRight
            -- You can add more detailed checks once FromJSON is implemented
     describe "Mesocycle domain model" $ do
        it "constructs a Mesocycle with correct structure" $ do
          let meso = Mesocycle
                { template = undefined  -- You can use a real template if available
                , numWeeks = 4
                , weeks = [ MesocycleWeek 1 [ MesocycleWorkout "Push" [ MesocycleExercise "Bench Press" Chest 4 (Just 10) (Just (PreExerciseFeedback (Just NeverSore))) (Just (PostExerciseFeedback (Just NoPain) (Just ModeratePump) (Just PrettyGood))) ] ] ]
                }
          numWeeks meso `shouldBe` 4
          weekNumber (head (weeks meso)) `shouldBe` 1
          let firstExercise = case weeks meso of
                (MesocycleWeek _ (MesocycleWorkout _ (MesocycleExercise name _ _ _ _ _ : _) : _) : _) -> name
                _ -> ""
          firstExercise `shouldBe` "Bench Press"
          let firstSoreness = case weeks meso of
                (MesocycleWeek _ (MesocycleWorkout _ (MesocycleExercise _ _ _ _ (Just (PreExerciseFeedback (Just s))) _ : _) : _) : _) -> Just s
                _ -> Nothing
          firstSoreness `shouldBe` Just NeverSore

