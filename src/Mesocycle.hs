{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Mesocycle where

import GHC.Generics (Generic)
import Data.List (find)

import qualified WorkoutTemplate


-- Enum types for feedback

data Soreness = NeverSore | HealedAWhileAgo | HealedJustInTime | StillSore
  deriving (Show, Eq, Enum, Generic)

data JointPain = NoPain | LowPain | ModeratePain | HighPain
  deriving (Show, Eq, Enum, Generic)

data Pump = LowPump | ModeratePump | AmazingPump
  deriving (Show, Eq, Enum, Generic)

data Workload = Easy | PrettyGood | PushedLimits | TooMuch
  deriving (Show, Eq, Enum, Generic)

-- Updated feedback types

data PreExerciseFeedback = PreExerciseFeedback
  { soreness   :: Maybe Soreness
  } deriving (Show, Eq, Generic)

data PostExerciseFeedback = PostExerciseFeedback
  { jointPain  :: Maybe JointPain
  , pump       :: Maybe Pump
  , workload   :: Maybe Workload
  } deriving (Show, Eq, Generic)

data Mesocycle = Mesocycle
  { numWeeks      :: Int
  , weeks         :: [MesocycleWeek]
  } deriving (Show, Eq, Generic)

data MesocycleWeek = MesocycleWeek
  { weekNumber    :: Int
  , workouts      :: [MesocycleWorkout]
  } deriving (Show, Eq, Generic)

data MesocycleWorkout = MesocycleWorkout
  { workoutName   :: String
  , exercises     :: [MesocycleExercise]
  } deriving (Show, Eq, Generic)

data MesocycleExercise = MesocycleExercise
  { exerciseName     :: String
  , muscleGroup      :: WorkoutTemplate.MuscleGroup
  , prescribedSets   :: Int
  , performedSets    :: Maybe Int
  , prescribedReps   :: Maybe Int
  , performedReps    :: Maybe Int
  , preFeedback      :: Maybe PreExerciseFeedback
  , postFeedback     :: Maybe PostExerciseFeedback
  } deriving (Show, Eq, Generic)


generateMesocycle :: WorkoutTemplate.WorkoutTemplate -> Int -> Mesocycle
generateMesocycle mesocycleTemplate weeksCount = 
  Mesocycle
  { numWeeks = weeksCount
  , weeks = [ MesocycleWeek n (map toMesocycleWorkout (WorkoutTemplate.weekDays mesocycleTemplate)) | n <- [1..weeksCount] ]
  } -- For each weekday, create a Workout, based on the template
    where
        toMesocycleWorkout :: WorkoutTemplate.Workout -> MesocycleWorkout
        toMesocycleWorkout (WorkoutTemplate.Workout name templateExercises) = MesocycleWorkout
            { workoutName = name
            , exercises = map toMesocycleExercise templateExercises
            }

        toMesocycleExercise :: WorkoutTemplate.Exercise -> MesocycleExercise
        toMesocycleExercise (WorkoutTemplate.Exercise name targetMuscleGroup sets) = MesocycleExercise
            { exerciseName = name
            , muscleGroup = targetMuscleGroup
            , prescribedSets = sets
            , performedSets = Nothing
            , prescribedReps = Nothing
            , performedReps = Nothing
            , preFeedback = Nothing
            , postFeedback = Nothing
            }



-- | Find the next exercise that needs to be performed/logged.
-- Returns (weekNumber, workoutName, exercise)
findNextActiveExercise :: Mesocycle -> Maybe (Int, String, MesocycleExercise)
findNextActiveExercise (Mesocycle { weeks }) = find needsInput allExercises
  where
    allExercises = [ (weekNumber w, workoutName wo, ex)
                   | w <- weeks
                   , wo <- workouts w
                   , ex <- exercises wo
                   ]
    needsInput (_, _, ex) = performedSets ex == Nothing || performedReps ex == Nothing

-- | Given the current week and total training weeks, prescribe RIR.
prescribedRIR :: Int -> Int -> Int
prescribedRIR week totalWeeks
  | week < totalWeeks = max 0 (3 - (week - 1))
  | week == totalWeeks = 8  -- deload week
  | otherwise = error "Week out of range for prescribedRIR"

-- | Find the most recent completed set for a given exercise name.
findMostRecentCompleted :: Mesocycle -> String -> Maybe MesocycleExercise
findMostRecentCompleted (Mesocycle { weeks }) exName =
  let allExercises = [ ex
                     | w <- reverse weeks
                     , wo <- reverse (workouts w)
                     , ex <- reverse (exercises wo)
                     , exerciseName ex == exName
                     , performedSets ex /= Nothing
                     , performedReps ex /= Nothing
                     ]
  in case allExercises of
       (x:_) -> Just x
       []    -> Nothing



-- | Suggest the next prescription (e.g., reps or weight) based on previous value and a progression percentage.
--   For reps, round as needed. For weight, use roundToStep.
suggestNextPrescription :: Double -> Double -> Double
suggestNextPrescription prevValue percent =
  let increase = prevValue * (percent / 100)
  in prevValue + increase

-- | Round a value to the nearest step (e.g., 1.25 for kg plates)
roundToStep :: Double -> Double -> Double
roundToStep value step = fromIntegral (round (value / step)) * step


-- | Log a set for a given exercise in a given week/workout by name.
--   Optionally override the prescription (reps/weight) via mPrescribedReps 
--   Returns the updated Mesocycle.
logSet :: Mesocycle -> Int -> String -> String -> Maybe Int -> Int -> Int -> Mesocycle
logSet meso weekNum targetWorkoutName exName mPrescribedReps performedSets performedReps =
  meso { weeks = map updateWeek (weeks meso) }
  where
    updateWeek w
      | weekNumber w == weekNum = w { workouts = map updateWorkout (workouts w) }
      | otherwise = w
    updateWorkout wo
      | workoutName wo == targetWorkoutName = wo { exercises = map updateExercise (exercises wo) }
      | otherwise = wo
    updateExercise ex
      | exerciseName ex == exName = ex
          { prescribedReps = mPrescribedReps <|> prescribedReps ex
          , performedSets = Just performedSets
          , performedReps = Just performedReps
          }
      | otherwise = ex
    -- (<|>) is from Alternative, but for Maybe it's just 'orElse'.
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y  = y

-- | Log pre-exercise feedback (soreness) for a given exercise in a given week/workout by name.
logPreExerciseFeedback :: Mesocycle -> Int -> String -> String -> Soreness -> Mesocycle
logPreExerciseFeedback meso weekNum targetWorkoutName exName sorenessVal =
  meso { weeks = map updateWeek (weeks meso) }
  where
    updateWeek w
      | weekNumber w == weekNum = w { workouts = map updateWorkout (workouts w) }
      | otherwise = w
    updateWorkout wo
      | workoutName wo == targetWorkoutName = wo { exercises = map updateExercise (exercises wo) }
      | otherwise = wo
    updateExercise ex
      | exerciseName ex == exName = ex { preFeedback = Just (PreExerciseFeedback (Just sorenessVal)) }
      | otherwise = ex

-- | Log post-exercise feedback (joint pain, pump, workload) for a given exercise in a given week/workout by name.
logPostExerciseFeedback :: Mesocycle -> Int -> String -> String -> JointPain -> Pump -> Workload -> Mesocycle
logPostExerciseFeedback meso weekNum targetWorkoutName exName jointPainVal pumpVal workloadVal =
  meso { weeks = map updateWeek (weeks meso) }
  where
    updateWeek w
      | weekNumber w == weekNum = w { workouts = map updateWorkout (workouts w) }
      | otherwise = w
    updateWorkout wo
      | workoutName wo == targetWorkoutName = wo { exercises = map updateExercise (exercises wo) }
      | otherwise = wo
    updateExercise ex
      | exerciseName ex == exName = ex { postFeedback = Just (PostExerciseFeedback (Just jointPainVal) (Just pumpVal) (Just workloadVal)) }
      | otherwise = ex
