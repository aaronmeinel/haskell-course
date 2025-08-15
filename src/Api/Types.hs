{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), withObject, object)
import qualified Mesocycle as Domain
import qualified WorkoutTemplate ()
-- (ToJSON already imported above)

-- Version response type (moved here for central export)
newtype VersionResponse = VersionResponse { apiVersion :: Int }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- (no extra imports needed currently)

-- Logging DTOs
data ExerciseLogRequest = ExerciseLogRequest
  { week :: Int
  , workoutIndex :: Int
  , exerciseIndex :: Int
  , loggedSets :: Int
  , loggedReps :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Log a single set (weight + reps) within an exercise
data SetLogRequest = SetLogRequest
  { setWeek :: Int
  , setWorkoutIndex :: Int
  , setExerciseIndex :: Int
  , setIndex :: Int -- 0-based from frontend
  , loggedWeight :: Double
  , setLoggedReps :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SetLogRequest where
  toJSON r = object
    [ "week" .= setWeek r
    , "workoutIndex" .= setWorkoutIndex r
    , "exerciseIndex" .= setExerciseIndex r
    , "setIndex" .= setIndex r
    , "loggedWeight" .= loggedWeight r
    , "loggedReps" .= setLoggedReps r
    ]

instance FromJSON SetLogRequest where
  parseJSON = withObject "SetLogRequest" $ \o -> SetLogRequest
      <$> o .: "week"
      <*> o .: "workoutIndex"
      <*> o .: "exerciseIndex"
      <*> o .: "setIndex"
      <*> o .: "loggedWeight"
      <*> o .: "loggedReps"

data LogResponse = LogResponse
  { updated :: Bool
  , message :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- CamelCase fields already match our Haskell names, so default mapping is fine.
-- If later we need custom mapping we can adjust.

-- DTOs intentionally exclude performed* and feedback fields 
data ExerciseDTO = ExerciseDTO
  { exerciseName :: String
  , muscleGroup :: String
  , prescribedSets :: Int
  , prescribedReps :: Maybe Int
  , performedSets :: Int  -- derived count
  , performedReps :: Maybe Int -- last logged reps
  , sets :: [SetPerfDTO]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SetPerfDTO = SetPerfDTO
  { weight :: Maybe Double
  , reps :: Maybe Int
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
  , prescribedReps = fmap Domain.unReps (Domain.prescribedReps ex)
  , performedSets = Domain.completedSets ex
  , performedReps = fmap Domain.unReps (Domain.lastLoggedReps ex)
  , sets = map fromDomainSet (Domain.setPerformances ex)
  }

fromDomainSet :: Domain.SetPerformance -> SetPerfDTO
fromDomainSet sp = SetPerfDTO { weight = fmap Domain.unWeight (Domain.weight sp)
                              , reps = fmap Domain.unReps (Domain.reps sp) }

fromDomainWorkout :: Domain.MesocycleWorkout -> WorkoutDTO
fromDomainWorkout w = WorkoutDTO
  { workoutName = Domain.workoutName w
  , exercises = map fromDomainExercise (Domain.exercises w)
  }

fromDomainWeek :: Domain.MesocycleWeek -> WeekDTO
fromDomainWeek w = WeekDTO
  { weekNumber = Domain.unWeekNumber (Domain.weekNumber w)
  , workouts = map fromDomainWorkout (Domain.workouts w)
  }

fromDomainPlan :: Domain.Mesocycle -> PlanDTO
fromDomainPlan m = PlanDTO
  { numWeeks = Domain.numWeeks m
  , weeks = map fromDomainWeek (Domain.weeks m)
  }
