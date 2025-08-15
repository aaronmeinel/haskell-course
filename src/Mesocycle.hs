{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Mesocycle where

import GHC.Generics (Generic)
import Data.List (find)
import Data.Aeson (ToJSON, FromJSON)

import qualified WorkoutTemplate

-- Newtypes for stronger type safety (Phase 1)
newtype WeekNumber = WeekNumber { unWeekNumber :: Int }
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Reps = Reps { unReps :: Int }
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Weight = Weight { unWeight :: Double }
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Real, Fractional)


-- Enum types for feedback

data Soreness = NeverSore | HealedAWhileAgo | HealedJustInTime | StillSore
  deriving (Show, Eq, Enum, Generic)

data JointPain = NoPain | LowPain | ModeratePain | HighPain
  deriving (Show, Eq, Enum, Generic)

data Pump = LowPump | ModeratePump | AmazingPump
  deriving (Show, Eq, Enum, Generic)

data Workload = Easy | PrettyGood | PushedLimits | TooMuch
  deriving (Show, Eq, Enum, Generic)



instance ToJSON Soreness
instance FromJSON Soreness
instance ToJSON JointPain
instance FromJSON JointPain
instance ToJSON Pump
instance FromJSON Pump
instance ToJSON Workload
instance FromJSON Workload



data PreExerciseFeedback = PreExerciseFeedback
  { soreness   :: Maybe Soreness
  } deriving (Show, Eq, Generic)

data PostExerciseFeedback = PostExerciseFeedback
  { jointPain  :: Maybe JointPain
  , pump       :: Maybe Pump
  , workload   :: Maybe Workload
  } deriving (Show, Eq, Generic)

instance ToJSON PreExerciseFeedback
instance FromJSON PreExerciseFeedback
instance ToJSON PostExerciseFeedback
instance FromJSON PostExerciseFeedback



instance ToJSON Mesocycle
instance FromJSON Mesocycle
instance ToJSON MesocycleWeek
instance FromJSON MesocycleWeek
instance ToJSON MesocycleWorkout
instance FromJSON MesocycleWorkout
instance ToJSON MesocycleExercise
instance FromJSON MesocycleExercise
instance ToJSON WeekNumber
instance FromJSON WeekNumber
instance ToJSON Reps
instance FromJSON Reps
instance ToJSON Weight
instance FromJSON Weight


data Mesocycle = Mesocycle
  { numWeeks      :: Int
  , weeks         :: [MesocycleWeek]
  } deriving (Show, Eq, Generic)

data MesocycleWeek = MesocycleWeek
  { weekNumber    :: WeekNumber
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
  , prescribedReps   :: Maybe Reps
  , preFeedback      :: Maybe PreExerciseFeedback
  , postFeedback     :: Maybe PostExerciseFeedback
  , setPerformances  :: [SetPerformance] -- ^ Per-set logged weight/reps (length = prescribedSets)
  } deriving (Show, Eq, Generic)

-- | Per-set performance (weight & reps may be logged independently later if needed)
data SetPerformance = SetPerformance
  { weight :: Maybe Weight
  , reps   :: Maybe Reps
  } deriving (Show, Eq, Generic)

instance ToJSON SetPerformance
instance FromJSON SetPerformance


generateMesocycle :: WorkoutTemplate.WorkoutTemplate -> Int -> Mesocycle
generateMesocycle mesocycleTemplate weeksCount = 
  Mesocycle
  { numWeeks = weeksCount
  , weeks = [ MesocycleWeek (WeekNumber n) (map toMesocycleWorkout (WorkoutTemplate.weekDays mesocycleTemplate)) | n <- [1..weeksCount] ]
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
          , prescribedReps = Nothing
          , preFeedback = Nothing
          , postFeedback = Nothing
          , setPerformances = replicate sets (SetPerformance Nothing Nothing)
          }



-- | Find the next exercise that needs to be performed/logged.
-- Returns (weekNumber, workoutName, exercise)
findNextActiveExercise :: Mesocycle -> Maybe (Int, String, MesocycleExercise)
findNextActiveExercise (Mesocycle { weeks }) = find (needsInput . thd) allExercises
  where
  allExercises = [ (unWeekNumber (weekNumber w), workoutName wo, ex)
                   | w <- weeks
                   , wo <- workouts w
                   , ex <- exercises wo
                   ]
  thd (_,_,x) = x
  needsInput ex = not (exerciseComplete ex)

-- | Given the current week and total training weeks, prescribe RIR.
prescribedRIR :: WeekNumber -> Int -> Int
prescribedRIR week totalWeeks
  | w < 1 || w > totalWeeks = error "Week out of range for prescribedRIR"
  | w < totalWeeks = max 0 (3 - (w - 1))  -- simple ramp: 3,2,1,...
  | otherwise = 0
  where w = unWeekNumber week

-- | Find the most recent completed set for a given exercise name.
findMostRecentCompleted :: Mesocycle -> String -> Maybe MesocycleExercise
findMostRecentCompleted (Mesocycle { weeks }) exName =
  let allExercises = [ ex
                     | w <- reverse weeks
                     , wo <- reverse (workouts w)
                     , ex <- reverse (exercises wo)
                     , exerciseName ex == exName
                     , exerciseComplete ex
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
roundToStep value step = (fromInteger (round (value / step))) * step

-- Derived / helper computations (no stored duplication)

-- | Count sets that have reps logged (weight optional for completion criteria)
completedSets :: MesocycleExercise -> Int
completedSets ex = length [ () | SetPerformance { reps = Just _ } <- setPerformances ex ]

-- | Determine if an exercise is complete (all prescribed sets have reps)
exerciseComplete :: MesocycleExercise -> Bool
exerciseComplete ex = completedSets ex >= prescribedSets ex

-- | Last logged reps among set performances
lastLoggedReps :: MesocycleExercise -> Maybe Reps
lastLoggedReps ex = case [ r | SetPerformance { reps = Just r } <- setPerformances ex ] of
  [] -> Nothing
  rs -> Just (last rs)

