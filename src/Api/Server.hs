{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Server (app) where

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Api.Routes
import Api.Types
import qualified Mesocycle
import qualified WorkoutTemplate
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Control.Monad.IO.Class (liftIO)

-- For now we generate a trivial plan each server start.
-- Later: load from file or configuration.

-- Global in-memory mutable mesocycle (simple dev prototype; not for production)
{-# NOINLINE globalPlanRef #-}
globalPlanRef :: IORef Mesocycle.Mesocycle
globalPlanRef = unsafePerformIO (newIORef initialPlan)

initialPlan :: Mesocycle.Mesocycle
initialPlan = Mesocycle.generateMesocycle sampleTemplate 4
  where
    sampleTemplate = WorkoutTemplate.WorkoutTemplate
      [ pushDay, pullDay, legsDay ]
    pushDay = WorkoutTemplate.Workout
      { WorkoutTemplate.workoutName = "Push"
      , WorkoutTemplate.exercises =
          [ WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Bench Press", WorkoutTemplate.muscleGroup = WorkoutTemplate.Chest, WorkoutTemplate.initialSets = 3 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Overhead Press", WorkoutTemplate.muscleGroup = WorkoutTemplate.Shoulders, WorkoutTemplate.initialSets = 3 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Cable Fly", WorkoutTemplate.muscleGroup = WorkoutTemplate.Chest, WorkoutTemplate.initialSets = 2 }
          ]
      }
    pullDay = WorkoutTemplate.Workout
      { WorkoutTemplate.workoutName = "Pull"
      , WorkoutTemplate.exercises =
          [ WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Deadlift", WorkoutTemplate.muscleGroup = WorkoutTemplate.Back, WorkoutTemplate.initialSets = 2 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Lat Pulldown", WorkoutTemplate.muscleGroup = WorkoutTemplate.Back, WorkoutTemplate.initialSets = 3 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Barbell Row", WorkoutTemplate.muscleGroup = WorkoutTemplate.Back, WorkoutTemplate.initialSets = 3 }
          ]
      }
    legsDay = WorkoutTemplate.Workout
      { WorkoutTemplate.workoutName = "Legs"
      , WorkoutTemplate.exercises =
          [ WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Back Squat", WorkoutTemplate.muscleGroup = WorkoutTemplate.Quads, WorkoutTemplate.initialSets = 3 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Romanian Deadlift", WorkoutTemplate.muscleGroup = WorkoutTemplate.Hamstrings, WorkoutTemplate.initialSets = 3 }
          , WorkoutTemplate.Exercise { WorkoutTemplate.exerciseName = "Leg Extension", WorkoutTemplate.muscleGroup = WorkoutTemplate.Quads, WorkoutTemplate.initialSets = 2 }
          ]
      }

-- Extend API with static file serving at root
type FullAPI = RootAPI :<|> Raw

serverRoot :: Server RootAPI
serverRoot = versionH :<|> planH :<|> logH
  where
    versionH = pure (VersionResponse 1)
    planH = do
      p <- liftIO (readIORef globalPlanRef)
      pure (fromDomainPlan p)
    logH req = do
      _ <- liftIO $ atomicModifyIORef' globalPlanRef (\p -> (applyLog req p, ()))
      pure (LogResponse True "Logged")

server :: Server FullAPI
server = serverRoot :<|> serveDirectoryFileServer "dist"

app :: Application
app = simpleCors $ serve (Proxy :: Proxy FullAPI) server

-- Apply an ExerciseLogRequest to a Mesocycle (naive traversal)
applyLog :: ExerciseLogRequest -> Mesocycle.Mesocycle -> Mesocycle.Mesocycle
applyLog req m = m { Mesocycle.weeks = map updateWeek (Mesocycle.weeks m) }
  where
    updateWeek w
      | Mesocycle.weekNumber w /= week req = w
      | otherwise = w { Mesocycle.workouts = mapWithIndex updateWorkout (Mesocycle.workouts w) }
    updateWorkout i wo
      | i /= workoutIndex req = wo
      | otherwise = wo { Mesocycle.exercises = mapWithIndex updateExercise (Mesocycle.exercises wo) }
    updateExercise j ex
      | j /= exerciseIndex req = ex
      | otherwise =
      let ExerciseLogRequest{ loggedSets = ls, loggedReps = lr } = req
      in ex { Mesocycle.performedSets = Just ls
        , Mesocycle.performedReps = Just lr
                }
    mapWithIndex f = zipWith f [0..]
