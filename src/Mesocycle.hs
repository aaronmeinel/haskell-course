module Mesocycle where

import WorkoutTemplate

-- Enum types for feedback

data Soreness = NeverSore | HealedAWhileAgo | HealedJustInTime | StillSore
  deriving (Show, Eq, Enum)

data JointPain = NoPain | LowPain | ModeratePain | HighPain
  deriving (Show, Eq, Enum)

data Pump = LowPump | ModeratePump | AmazingPump
  deriving (Show, Eq, Enum)

data Workload = Easy | PrettyGood | PushedLimits | TooMuch
  deriving (Show, Eq, Enum)

-- Updated feedback types

data PreExerciseFeedback = PreExerciseFeedback
  { soreness   :: Maybe Soreness
  } deriving (Show, Eq)

data PostExerciseFeedback = PostExerciseFeedback
  { jointPain  :: Maybe JointPain
  , pump       :: Maybe Pump
  , workload   :: Maybe Workload
  } deriving (Show, Eq)

data Mesocycle = Mesocycle
  { template      :: WorkoutTemplate
  , numWeeks      :: Int
  , weeks         :: [MesocycleWeek]
  } deriving (Show, Eq)

data MesocycleWeek = MesocycleWeek
  { weekNumber    :: Int
  , workouts      :: [MesocycleWorkout]
  } deriving (Show, Eq)

data MesocycleWorkout = MesocycleWorkout
  { workoutName   :: String
  , exercises     :: [MesocycleExercise]
  } deriving (Show, Eq)

data MesocycleExercise = MesocycleExercise
  { exerciseName     :: String
  , muscleGroup      :: MuscleGroup
  , prescribedSets   :: Int
  , prescribedReps   :: Maybe Int
  , preFeedback      :: Maybe PreExerciseFeedback
  , postFeedback     :: Maybe PostExerciseFeedback
  } deriving (Show, Eq)
