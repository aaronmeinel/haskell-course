module WorkoutLogOpsSpec where

import Test.Hspec
import Data.Time (UTCTime)
import WorkoutLog
import WorkoutLogOps

spec :: Spec
spec = do
  describe "findMostRecentSetLog" $ do
    let set1 = SetLog { setWeekNumber = 1, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 1, prescribedWeight = Just 100, performedWeight = 100, prescribedReps = Just 10, performedReps = 10, setTimestamp = Nothing }
        set2 = SetLog { setWeekNumber = 1, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 2, prescribedWeight = Just 100, performedWeight = 100, prescribedReps = Just 10, performedReps = 9, setTimestamp = Nothing }
        set3 = SetLog { setWeekNumber = 2, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 1, prescribedWeight = Just 102.5, performedWeight = 102.5, prescribedReps = Just 10, performedReps = 10, setTimestamp = Nothing }
        wlog = WorkoutLog [set1, set2, set3] []
    it "finds the most recent set for an exercise (any set index)" $ do
      findMostRecentSetLog wlog "Push" "Bench Press" Nothing `shouldBe` Just set3
    it "finds the most recent set for a specific set index" $ do
      findMostRecentSetLog wlog "Push" "Bench Press" (Just 2) `shouldBe` Just set2
    it "returns Nothing if no set matches" $ do
      findMostRecentSetLog wlog "Push" "Squat" Nothing `shouldBe` Nothing

  describe "suggestNextPrescriptionFromLog" $ do
    let set1 = SetLog { setWeekNumber = 1, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 1, prescribedWeight = Just 100, performedWeight = 100, prescribedReps = Just 10, performedReps = 10, setTimestamp = Nothing }
        set2 = SetLog { setWeekNumber = 1, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 2, prescribedWeight = Just 100, performedWeight = 100, prescribedReps = Just 10, performedReps = 9, setTimestamp = Nothing }
        set3 = SetLog { setWeekNumber = 2, setWorkoutName = "Push", setExerciseName = "Bench Press", setIndex = 1, prescribedWeight = Just 102.5, performedWeight = 102.5, prescribedReps = Just 10, performedReps = 10, setTimestamp = Nothing }
        wlog = WorkoutLog [set1, set2, set3] []
    it "suggests increased weight if last set was successful (approximate)" $ do
      let (nextWeight, nextReps) = suggestNextPrescriptionFromLog wlog "Push" "Bench Press" (Just 1) 2 2.5
      -- 102.5 * 1.025 = 105.0625 (previous weight * (1 + increment/100))
      nextWeight `shouldSatisfy` (\w -> abs (w - 105.0625) < 1e-6)
      nextReps `shouldBe` 10
    it "suggests same weight if last set was not successful" $ do
      let (nextWeight, nextReps) = suggestNextPrescriptionFromLog wlog "Push" "Bench Press" (Just 2) 2 2.5
      nextWeight `shouldBe` 100
      nextReps `shouldBe` 10
    it "returns (0,0) if no history" $ do
      let (nextWeight, nextReps) = suggestNextPrescriptionFromLog wlog "Push" "Squat" Nothing 2 2.5
      nextWeight `shouldBe` 0
      nextReps `shouldBe` 0
