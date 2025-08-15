{-# LANGUAGE NamedFieldPuns #-}
module WorkoutLogOps where

import Mesocycle
import WorkoutLog
import Data.Time (UTCTime)

-- | Log a set: looks up prescription in Mesocycle, records performed values in WorkoutLog
logSetWithPlan :: Mesocycle -> WorkoutLog -> Int -> String -> String -> Int -> Double -> Int -> Maybe UTCTime -> WorkoutLog
logSetWithPlan meso wlog weekNum workoutName exName setIdx performedWeight performedReps mTime =
  let (prescribedWeight, prescribedReps) = lookupPrescription meso weekNum workoutName exName setIdx
      setLog = SetLog
        { setWeekNumber = weekNum
        , setWorkoutName = workoutName
        , setExerciseName = exName
        , setIndex = setIdx
        , prescribedWeight = prescribedWeight
        , WorkoutLog.prescribedReps = prescribedReps
        , performedWeight = performedWeight
        , WorkoutLog.performedReps = performedReps
        , setTimestamp = mTime
        }
  in wlog { setLogs = setLogs wlog ++ [setLog] }

-- | Log feedback: records pre- or post-exercise feedback in WorkoutLog
logFeedbackWithPlan :: Mesocycle -> WorkoutLog -> Int -> String -> String -> Maybe PreExerciseFeedback -> Maybe PostExerciseFeedback -> WorkoutLog
logFeedbackWithPlan _ wlog weekNum workoutName exName preF postF =
  let feedbackLog = FeedbackLog
        { feedbackWeekNumber = weekNum
        , feedbackWorkoutName = workoutName
        , feedbackExerciseName = exName
        , WorkoutLog.preFeedback = preF
        , WorkoutLog.postFeedback = postF
        }
  in wlog { feedbackLogs = feedbackLogs wlog ++ [feedbackLog] }

-- | Helper: look up prescription for a set in the Mesocycle plan
lookupPrescription :: Mesocycle -> Int -> String -> String -> Int -> (Maybe Double, Maybe Int)
lookupPrescription (Mesocycle { weeks }) weekNum wName exName _setIdx =
  case filter ((== WeekNumber weekNum) . weekNumber) weeks of
    (w:_) -> case filter ((== wName) . workoutName) (workouts w) of
      (wo:_) -> case filter ((== exName) . exerciseName) (exercises wo) of
        (ex:_) -> (Nothing, fmap unReps (Mesocycle.prescribedReps ex)) -- No prescribed weight in current model
        _ -> (Nothing, Nothing)
      _ -> (Nothing, Nothing)
    _ -> (Nothing, Nothing)

-- | Find the most recent SetLog for a given exercise (and optionally set index)
findMostRecentSetLog :: WorkoutLog -> String -> String -> Maybe Int -> Maybe SetLog
findMostRecentSetLog wlog workoutName exName mSetIdx =
  let matches sl = workoutName == setWorkoutName sl && exName == setExerciseName sl && maybe True (== setIndex sl) mSetIdx
  in case filter matches (reverse (setLogs wlog)) of
       (sl:_) -> Just sl
       []     -> Nothing

-- | Suggest the next prescription (weight, reps) for an exercise based on log history and target RIR
--   This is a simple example: if last set was successful, increase weight by percent; otherwise, keep the same or decrease
suggestNextPrescriptionFromLog :: WorkoutLog -> String -> String -> Maybe Int -> Int -> Double -> (Double, Int)
suggestNextPrescriptionFromLog wlog workoutName exName mSetIdx targetRIR percentIncrease =
  case findMostRecentSetLog wlog workoutName exName mSetIdx of
    Just lastSet ->
      let lastWeight = performedWeight lastSet
          lastReps   = WorkoutLog.performedReps lastSet
          -- Simple rule: if lastReps >= prescribedReps and RIR was hit, increase weight
          nextWeight = case WorkoutLog.prescribedReps lastSet of
                         Just repsTarget | lastReps >= repsTarget -> lastWeight * (1 + percentIncrease / 100)
                         _                                        -> lastWeight
          -- Suggest reps based on target RIR (could be more sophisticated)
          nextReps = maybe lastReps id (WorkoutLog.prescribedReps lastSet)
      in (nextWeight, nextReps)
    Nothing -> (0, 0) -- No history, fallback to 0s or could use plan
