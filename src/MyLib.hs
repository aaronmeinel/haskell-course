{-# LANGUAGE OverloadedStrings #-}
module MyLib (
  WorkoutTemplate(..),
  Workout(..),
  Exercise(..),
  MuscleGroup(..)
) where

import Data.Yaml

import qualified Data.Text as T


-- Represents a muscle group, e.g., Chest, Back, Hamstrings
data MuscleGroup = Chest | Back | Hamstrings | Quads | Shoulders | Arms | Calves | Abs
  deriving (Show, Read, Eq)

-- Represents an exercise, e.g., Bench Press, Squat
data Exercise = Exercise
  { exerciseName   :: String
  , muscleGroup    :: MuscleGroup
  , initialSets           :: Int
} deriving (Show, Eq)

-- Represents a workout, e.g., "Monday Push"
data Workout = Workout
  { workoutName    :: String
  , exercises      :: [Exercise]
  } deriving (Show, Eq)


-- Represents a template for a week of workouts
data WorkoutTemplate = WorkoutTemplate
  { weekDays       :: [Workout]  -- Each workout assigned to a day
  } deriving (Show, Eq)



instance FromJSON MuscleGroup where
  parseJSON = withText "MuscleGroup" $ \t -> case T.unpack t of
    "Chest"      -> pure Chest
    "Back"       -> pure Back
    "Hamstrings" -> pure Hamstrings
    "Quads"      -> pure Quads
    "Shoulders"  -> pure Shoulders
    "Arms"       -> pure Arms
    "Calves"     -> pure Calves
    "Abs"        -> pure Abs
    _            -> fail $ "Unknown muscle group: " ++ T.unpack t


instance FromJSON Exercise where
  parseJSON = withObject "Exercise" $ \v -> Exercise
    <$> v .: "name"
    <*> v .: "muscle_group"
    <*> v .: "initial_sets"

instance FromJSON Workout where
  parseJSON = withObject "Workout" $ \v -> Workout
    <$> v .: "name"
    <*> v .: "exercises"

instance FromJSON WorkoutTemplate where
  parseJSON = withObject "WorkoutTemplate" $ \v -> WorkoutTemplate
    <$> v .: "week_days"
