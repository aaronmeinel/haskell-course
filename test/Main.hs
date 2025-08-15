module Main where

import Test.Hspec
import Data.Yaml (decodeFileEither, ParseException)
import qualified WorkoutTemplate
import qualified Mesocycle
import MesocyclePersistence (saveMesocycle, loadMesocycle)
import System.Directory (removeFile, getTemporaryDirectory)
import qualified WorkoutLogOpsSpec

mkExercise :: String -> WorkoutTemplate.MuscleGroup -> Int -> Maybe Int -> Maybe Int -> Mesocycle.MesocycleExercise
mkExercise name grp sets presReps perfReps = Mesocycle.MesocycleExercise
  { Mesocycle.exerciseName = name
  , Mesocycle.muscleGroup = grp
  , Mesocycle.prescribedSets = sets
  , Mesocycle.performedSets = if perfReps /= Nothing then Just sets else Nothing
  , Mesocycle.prescribedReps = presReps
  , Mesocycle.performedReps = perfReps
  , Mesocycle.preFeedback = Nothing
  , Mesocycle.postFeedback = Nothing
  , Mesocycle.setPerformances = replicate sets (Mesocycle.SetPerformance Nothing Nothing)
  }

main :: IO ()
main = hspec $ do
  describe "WorkoutTemplate YAML parsing" $ do
    it "parses a valid workout_template.yaml file" $ do
      e <- decodeFileEither "workout_template.yaml" :: IO (Either ParseException WorkoutTemplate.WorkoutTemplate)
      case e of
        Left _ -> expectationFailure "Failed to parse workout_template.yaml"
        Right _ -> pure ()

  describe "Mesocycle domain model" $ do
    it "constructs a Mesocycle with correct structure" $ do
      let benchPress = (mkExercise "Bench Press" WorkoutTemplate.Chest 4 (Just 10) Nothing)
            { Mesocycle.preFeedback = Just (Mesocycle.PreExerciseFeedback (Just Mesocycle.NeverSore))
            , Mesocycle.postFeedback = Just (Mesocycle.PostExerciseFeedback (Just Mesocycle.NoPain) (Just Mesocycle.ModeratePump) (Just Mesocycle.PrettyGood))
            }
          pushWorkout = Mesocycle.MesocycleWorkout "Push" [benchPress]
          week1 = Mesocycle.MesocycleWeek 1 [pushWorkout]
          meso = Mesocycle.Mesocycle 4 [week1]
      Mesocycle.numWeeks meso `shouldBe` 4
      Mesocycle.weekNumber (head (Mesocycle.weeks meso)) `shouldBe` 1
      Mesocycle.exerciseName (head . Mesocycle.exercises . head . Mesocycle.workouts $ head (Mesocycle.weeks meso)) `shouldBe` "Bench Press"
      Mesocycle.preFeedback benchPress `shouldBe` Just (Mesocycle.PreExerciseFeedback (Just Mesocycle.NeverSore))

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
      Mesocycle.preFeedback firstExercise `shouldBe` Nothing
      Mesocycle.postFeedback firstExercise `shouldBe` Nothing

  describe "MesocyclePersistence" $ do
    it "round-trips a Mesocycle to JSON and back" $ do
      let benchPress = mkExercise "Bench Press" WorkoutTemplate.Chest 4 (Just 10) Nothing
          pushWorkout = Mesocycle.MesocycleWorkout "Push" [benchPress]
          week1 = Mesocycle.MesocycleWeek 1 [pushWorkout]
          meso = Mesocycle.Mesocycle 1 [week1]
      path <- fmap (++ "/test_mesocycle.json") getTemporaryDirectory
      saveMesocycle path meso
      loaded <- loadMesocycle path
      removeFile path
      loaded `shouldBe` Just meso

  describe "findNextActiveExercise" $ do
    it "finds the first exercise needing input" $ do
      let ex1 = (mkExercise "Bench Press" WorkoutTemplate.Chest 3 (Just 10) (Just 10))
          ex2 = mkExercise "Squat" WorkoutTemplate.Quads 4 (Just 8) Nothing
          workout = Mesocycle.MesocycleWorkout "Day 1" [ex1, ex2]
          week = Mesocycle.MesocycleWeek 1 [workout]
          meso = Mesocycle.Mesocycle 1 [week]
      Mesocycle.findNextActiveExercise meso `shouldBe` Just (1, "Day 1", ex2)

    it "returns Nothing if all exercises are complete" $ do
      let ex1 = (mkExercise "Bench Press" WorkoutTemplate.Chest 3 (Just 10) (Just 10))
          workout = Mesocycle.MesocycleWorkout "Day 1" [ex1]
          week = Mesocycle.MesocycleWeek 1 [workout]
          meso = Mesocycle.Mesocycle 1 [week]
      Mesocycle.findNextActiveExercise meso `shouldBe` Nothing

  describe "prescribedRIR" $ do
    it "prescribes RIR 3 for week 1 of 4" $ Mesocycle.prescribedRIR 1 4 `shouldBe` 3
    it "prescribes RIR 2 for week 2 of 4" $ Mesocycle.prescribedRIR 2 4 `shouldBe` 2
    it "prescribes RIR 1 for week 3 of 4" $ Mesocycle.prescribedRIR 3 4 `shouldBe` 1
    it "prescribes RIR 0 for week 4 of 4 (deload)" $ Mesocycle.prescribedRIR 4 4 `shouldBe` 0

  describe "findMostRecentCompleted" $ do
    it "finds the most recent completed set for an exercise" $ do
      let ex1 = mkExercise "Bench Press" WorkoutTemplate.Chest 3 (Just 10) (Just 10)
          ex2 = mkExercise "Bench Press" WorkoutTemplate.Chest 3 (Just 12) (Just 12)
          ex3 = mkExercise "Squat" WorkoutTemplate.Quads 4 (Just 8) (Just 8)
          workout1 = Mesocycle.MesocycleWorkout "Day 1" [ex1, ex3]
          workout2 = Mesocycle.MesocycleWorkout "Day 2" [ex2]
          week1 = Mesocycle.MesocycleWeek 1 [workout1]
          week2 = Mesocycle.MesocycleWeek 2 [workout2]
          meso = Mesocycle.Mesocycle 2 [week1, week2]
      Mesocycle.findMostRecentCompleted meso "Bench Press" `shouldBe` Just ex2
      Mesocycle.findMostRecentCompleted meso "Squat" `shouldBe` Just ex3
      Mesocycle.findMostRecentCompleted meso "Deadlift" `shouldBe` Nothing

    it "ignores incomplete sets" $ do
      let ex1 = mkExercise "Bench Press" WorkoutTemplate.Chest 3 (Just 10) Nothing
          workout = Mesocycle.MesocycleWorkout "Day 1" [ex1]
          week = Mesocycle.MesocycleWeek 1 [workout]
          meso = Mesocycle.Mesocycle 1 [week]
      Mesocycle.findMostRecentCompleted meso "Bench Press" `shouldBe` Nothing

  describe "suggestNextPrescription (Double)" $ do
    it "suggests a 2.5% increase from 100 as 102.5" $ Mesocycle.suggestNextPrescription 100 2.5 `shouldBe` 102.5
    it "suggests a 5% increase from 200 as 210.0" $ Mesocycle.suggestNextPrescription 200 5 `shouldBe` 210.0
    it "suggests a 0% increase returns the same value (Double)" $ Mesocycle.suggestNextPrescription 150 0 `shouldBe` 150.0
  it "can be used for reps by rounding to Int" $ (round (Mesocycle.suggestNextPrescription 99 2.5) :: Int) `shouldBe` 101

  WorkoutLogOpsSpec.spec

