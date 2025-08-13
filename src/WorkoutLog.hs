{-# LANGUAGE DeriveGeneric #-}
module WorkoutLog where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Mesocycle (PreExerciseFeedback, PostExerciseFeedback)

-- Log of a single set performed by the user
-- setIndex is 1-based (first set = 1)
data SetLog = SetLog
  { setWeekNumber      :: Int
  , setWorkoutName     :: String
  , setExerciseName    :: String
  , setIndex        :: Int
  , prescribedWeight :: Maybe Double
  , performedWeight :: Double
  , prescribedReps   :: Maybe Int
  , performedReps    :: Int
  , setTimestamp       :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- Feedback for an exercise (pre and post)
data FeedbackLog = FeedbackLog
  { feedbackWeekNumber      :: Int
  , feedbackWorkoutName     :: String
  , feedbackExerciseName    :: String
  , preFeedback     :: Maybe PreExerciseFeedback
  , postFeedback    :: Maybe PostExerciseFeedback
  } deriving (Show, Eq, Generic)

-- All logs for a workout session
data WorkoutLog = WorkoutLog
  { setLogs      :: [SetLog]
  , feedbackLogs :: [FeedbackLog]
  } deriving (Show, Eq, Generic)

-- All logs for a user (could be split by mesocycle, etc.)
type UserLog = [WorkoutLog]
