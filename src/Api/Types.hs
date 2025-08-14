{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Mesocycle as Domain
import qualified WorkoutTemplate

-- CamelCase fields already match our Haskell names, so default mapping is fine.
-- If later we need custom mapping we can adjust.

-- DTOs intentionally exclude performed* and feedback fields (separate endpoint later)

data ExerciseDTO = ExerciseDTO
  { exerciseName :: String
  , muscleGroup :: String
  , prescribedSets :: Int
  , prescribedReps :: Maybe Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WorkoutDTO = WorkoutDTO
  { workoutName :: String
  , exercises :: [ExerciseDTO]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WeekDTO = WeekDTO
  { weekNumber :: Int
  , workouts :: [WorkoutDTO]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PlanDTO = PlanDTO
  { numWeeks :: Int
  , weeks :: [WeekDTO]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Conversion functions

fromDomainExercise :: Domain.MesocycleExercise -> ExerciseDTO
fromDomainExercise ex = ExerciseDTO
  { exerciseName = Domain.exerciseName ex
  , muscleGroup = show (Domain.muscleGroup ex)
  , prescribedSets = Domain.prescribedSets ex
  , prescribedReps = Domain.prescribedReps ex
  }

fromDomainWorkout :: Domain.MesocycleWorkout -> WorkoutDTO
fromDomainWorkout w = WorkoutDTO
  { workoutName = Domain.workoutName w
  , exercises = map fromDomainExercise (Domain.exercises w)
  }

fromDomainWeek :: Domain.MesocycleWeek -> WeekDTO
fromDomainWeek w = WeekDTO
  { weekNumber = Domain.weekNumber w
  , workouts = map fromDomainWorkout (Domain.workouts w)
  }

fromDomainPlan :: Domain.Mesocycle -> PlanDTO
fromDomainPlan m = PlanDTO
  { numWeeks = Domain.numWeeks m
  , weeks = map fromDomainWeek (Domain.weeks m)
  }
