module Main where

import Test.Hspec
import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isRight)
import qualified WorkoutTemplate
import qualified Mesocycle
import MesocyclePersistence (saveMesocycle, loadMesocycle)
import System.Directory (removeFile, getTemporaryDirectory)


main :: IO ()
main = hspec $ do
  describe "WorkoutTemplate YAML parsing" $ do
    it "parses a valid workout_template.yaml file" $ do
      result <- decodeFileEither "workout_template.yaml" :: IO (Either Data.Yaml.ParseException WorkoutTemplate.WorkoutTemplate)
      result `shouldSatisfy` isRight
      -- You can add more detailed checks once FromJSON is implemented

  describe "Mesocycle domain model" $ do
    it "constructs a Mesocycle with correct structure" $ do
      let benchPress = Mesocycle.MesocycleExercise
            { Mesocycle.exerciseName = "Bench Press"
            , Mesocycle.muscleGroup = WorkoutTemplate.Chest
            , Mesocycle.prescribedSets = 4
            , Mesocycle.performedSets = Nothing
            , Mesocycle.prescribedReps = Just 10
            , Mesocycle.performedReps = Nothing
            , Mesocycle.preFeedback = Just (Mesocycle.PreExerciseFeedback (Just Mesocycle.NeverSore))
            , Mesocycle.postFeedback = Just (Mesocycle.PostExerciseFeedback (Just Mesocycle.NoPain) (Just Mesocycle.ModeratePump) (Just Mesocycle.PrettyGood))
            }
          pushWorkout = Mesocycle.MesocycleWorkout
            { Mesocycle.workoutName = "Push"
            , Mesocycle.exercises = [benchPress]
            }
          week1 = Mesocycle.MesocycleWeek
            { Mesocycle.weekNumber = 1
            , Mesocycle.workouts = [pushWorkout]
            }
          meso = Mesocycle.Mesocycle
            { Mesocycle.numWeeks = 4
            , Mesocycle.weeks = [week1]
            }
      Mesocycle.numWeeks meso `shouldBe` 4
      Mesocycle.weekNumber (head (Mesocycle.weeks meso)) `shouldBe` 1
      Mesocycle.exerciseName (head . Mesocycle.exercises . head . Mesocycle.workouts $ head (Mesocycle.weeks meso)) `shouldBe` "Bench Press"
      let firstExercise = case Mesocycle.weeks meso of
            (Mesocycle.MesocycleWeek _ (Mesocycle.MesocycleWorkout _ (e : _) : _) : _) -> Mesocycle.exerciseName e
            _ -> ""
      firstExercise `shouldBe` "Bench Press"
      let firstSoreness = case Mesocycle.weeks meso of
            (Mesocycle.MesocycleWeek _ (Mesocycle.MesocycleWorkout _ (Mesocycle.MesocycleExercise _ _ _ _ _ _ (Just (Mesocycle.PreExerciseFeedback (Just s))) _ : _) : _) : _) -> Just s
            _ -> Nothing
      firstSoreness `shouldBe` Just Mesocycle.NeverSore

  describe "generateMesocycle" $ do
    it "creates a mesocycle with the correct number of weeks" $ do
      let template = WorkoutTemplate.WorkoutTemplate [WorkoutTemplate.Workout "Push" [WorkoutTemplate.Exercise "Bench Press" WorkoutTemplate.Chest 3]]
          meso = Mesocycle.generateMesocycle template 4
      Mesocycle.numWeeks meso `shouldBe` 4
      length (Mesocycle.weeks meso) `shouldBe` 4

    it "copies workout and exercise names from the template" $ do
      let template = WorkoutTemplate.WorkoutTemplate [WorkoutTemplate.Workout "Pull" [WorkoutTemplate.Exercise "Row" WorkoutTemplate.Back 2, WorkoutTemplate.Exercise "Curl" WorkoutTemplate.Arms 2]]
          meso = Mesocycle.generateMesocycle template 2
          firstWeek = head (Mesocycle.weeks meso)
          firstWorkout = head (Mesocycle.workouts firstWeek)
          exNames = map Mesocycle.exerciseName (Mesocycle.exercises firstWorkout)
      Mesocycle.workoutName firstWorkout `shouldBe` "Pull"
      exNames `shouldBe` ["Row", "Curl"]

    it "initializes feedback fields as Nothing" $ do
      let template = WorkoutTemplate.WorkoutTemplate [WorkoutTemplate.Workout "Legs" [WorkoutTemplate.Exercise "Squat" WorkoutTemplate.Quads 4]]
          meso = Mesocycle.generateMesocycle template 1
          firstExercise = case Mesocycle.weeks meso of
            (Mesocycle.MesocycleWeek _ (Mesocycle.MesocycleWorkout _ (e : _) : _) : _) -> e
            _ -> error "No exercises found"
          preFeedback = Mesocycle.preFeedback firstExercise
          postFeedback = Mesocycle.postFeedback firstExercise
      preFeedback `shouldBe` Nothing
      postFeedback  `shouldBe` Nothing

  describe "MesocyclePersistence" $ do
    it "round-trips a Mesocycle to JSON and back" $ do
      let benchPress = Mesocycle.MesocycleExercise
            { Mesocycle.exerciseName = "Bench Press"
            , Mesocycle.muscleGroup = WorkoutTemplate.Chest
            , Mesocycle.prescribedSets = 4
            , Mesocycle.performedSets = Nothing
            , Mesocycle.prescribedReps = Just 10
            , Mesocycle.performedReps = Nothing
            , Mesocycle.preFeedback = Just (Mesocycle.PreExerciseFeedback (Just Mesocycle.NeverSore))
            , Mesocycle.postFeedback = Just (Mesocycle.PostExerciseFeedback (Just Mesocycle.NoPain) (Just Mesocycle.ModeratePump) (Just Mesocycle.PrettyGood))
            }
          pushWorkout = Mesocycle.MesocycleWorkout
            { Mesocycle.workoutName = "Push"
            , Mesocycle.exercises = [benchPress]
            }
          week1 = Mesocycle.MesocycleWeek
            { Mesocycle.weekNumber = 1
            , Mesocycle.workouts = [pushWorkout]
            }
          meso = Mesocycle.Mesocycle
            { Mesocycle.numWeeks = 1
            , Mesocycle.weeks = [week1]
            }
      path <- fmap (++ "/test_mesocycle.json") getTemporaryDirectory
      saveMesocycle path meso
      loaded <- loadMesocycle path
      removeFile path
      loaded `shouldBe` Just meso

  describe "findNextActiveExercise" $ do
    it "finds the first exercise needing input" $ do
      let ex1 = Mesocycle.MesocycleExercise
            { Mesocycle.exerciseName = "Bench Press"
            , Mesocycle.muscleGroup = WorkoutTemplate.Chest
            , Mesocycle.prescribedSets = 3
            , Mesocycle.performedSets = Just 3
            , Mesocycle.prescribedReps = Just 10
            , Mesocycle.performedReps = Just 10
            , Mesocycle.preFeedback = Nothing
            , Mesocycle.postFeedback = Nothing
            }
          ex2 = Mesocycle.MesocycleExercise
            { Mesocycle.exerciseName = "Squat"
            , Mesocycle.muscleGroup = WorkoutTemplate.Quads
            , Mesocycle.prescribedSets = 4
            , Mesocycle.performedSets = Nothing
            , Mesocycle.prescribedReps = Just 8
            , Mesocycle.performedReps = Nothing
            , Mesocycle.preFeedback = Nothing
            , Mesocycle.postFeedback = Nothing
            }
          workout = Mesocycle.MesocycleWorkout
            { Mesocycle.workoutName = "Day 1"
            , Mesocycle.exercises = [ex1, ex2]
            }
          week = Mesocycle.MesocycleWeek
            { Mesocycle.weekNumber = 1
            , Mesocycle.workouts = [workout]
            }
          meso = Mesocycle.Mesocycle
            { Mesocycle.numWeeks = 1
            , Mesocycle.weeks = [week]
            }
      Mesocycle.findNextActiveExercise meso `shouldBe` Just (1, "Day 1", ex2)

    it "returns Nothing if all exercises are complete" $ do
      let ex1 = Mesocycle.MesocycleExercise
            { Mesocycle.exerciseName = "Bench Press"
            , Mesocycle.muscleGroup = WorkoutTemplate.Chest
            , Mesocycle.prescribedSets = 3
            , Mesocycle.performedSets = Just 3
            , Mesocycle.prescribedReps = Just 10
            , Mesocycle.performedReps = Just 10
            , Mesocycle.preFeedback = Nothing
            , Mesocycle.postFeedback = Nothing
            }
          workout = Mesocycle.MesocycleWorkout
            { Mesocycle.workoutName = "Day 1"
            , Mesocycle.exercises = [ex1]
            }
          week = Mesocycle.MesocycleWeek
            { Mesocycle.weekNumber = 1
            , Mesocycle.workouts = [workout]
            }
          meso = Mesocycle.Mesocycle
            { Mesocycle.numWeeks = 1
            , Mesocycle.weeks = [week]
            }
      Mesocycle.findNextActiveExercise meso `shouldBe` Nothing

  describe "prescribedRIR" $ do
    it "prescribes RIR 3 for week 1 of 4" $ do
      Mesocycle.prescribedRIR 1 4 `shouldBe` 3
    it "prescribes RIR 2 for week 2 of 4" $ do
      Mesocycle.prescribedRIR 2 4 `shouldBe` 2
    it "prescribes RIR 1 for week 3 of 4" $ do
      Mesocycle.prescribedRIR 3 4 `shouldBe` 1
    it "prescribes RIR 0 for week 4 of 4 (last training week)" $ do
      Mesocycle.prescribedRIR 4 4 `shouldBe` 8
    it "prescribes RIR 8 for deload week (week == totalWeeks)" $ do
      Mesocycle.prescribedRIR 4 4 `shouldBe` 8

