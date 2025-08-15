{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Server (app) where

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Api.Routes
import Api.Types ( ExerciseLogRequest(..)
                 , SetLogRequest(..)
                 , LogResponse(..)
                 , PlanDTO(..)
                 , VersionResponse(..)
                 , fromDomainPlan
                 )
import qualified Mesocycle
import qualified WorkoutTemplate
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Control.Monad.IO.Class (liftIO)
import qualified MesocyclePersistence
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Exception (catch, IOException)


{-# NOINLINE globalPlanRef #-}
globalPlanRef :: IORef Mesocycle.Mesocycle
globalPlanRef = unsafePerformIO (do
  createDirectoryIfMissing True dataDir
  exists <- doesFileExist persistFile
  plan <- if exists
            then do
              m <- MesocyclePersistence.loadMesocycle persistFile `catch` handleLoad
              case m of
                Just p -> pure p
                Nothing -> do
                  MesocyclePersistence.saveMesocycle persistFile initialPlan
                  pure initialPlan
            else do
              MesocyclePersistence.saveMesocycle persistFile initialPlan
              pure initialPlan
  newIORef plan)
  where
    handleLoad :: IOException -> IO (Maybe Mesocycle.Mesocycle)
    handleLoad _ = pure Nothing

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

type FullAPI = RootAPI :<|> Raw

serverRoot :: Server RootAPI
serverRoot = versionH :<|> planH :<|> logH :<|> logSetH
  where
    versionH = pure (VersionResponse 1)
    planH = do
      p <- liftIO (readIORef globalPlanRef)
      pure (fromDomainPlan p)
    logH req = do
      updated <- liftIO $ atomicModifyIORef' globalPlanRef (\p -> let p' = applyLog req p in (p', p'))
      liftIO $ MesocyclePersistence.saveMesocycle persistFile updated
      pure (LogResponse True "Logged")
    logSetH sreq = do
      updated <- liftIO $ atomicModifyIORef' globalPlanRef (\p -> let p' = applySetLog sreq p in (p', p'))
      liftIO $ MesocyclePersistence.saveMesocycle persistFile updated
      pure (LogResponse True "Set Logged")

server :: Server FullAPI
server = serverRoot :<|> serveDirectoryFileServer "dist"

app :: Application
app = simpleCors $ serve (Proxy :: Proxy FullAPI) server

applyLog :: ExerciseLogRequest -> Mesocycle.Mesocycle -> Mesocycle.Mesocycle
applyLog req@ExerciseLogRequest
  { week = wk
  , workoutIndex = wIx
  , exerciseIndex = eIx
  , loggedSets = ls
  , loggedReps = lr
  } m =
  m { Mesocycle.weeks = map updateWeek (Mesocycle.weeks m) }
  where
    updateWeek w
      | Mesocycle.weekNumber w /= wk = w
      | otherwise = w { Mesocycle.workouts = mapWithIndex updateWorkout (Mesocycle.workouts w) }

    updateWorkout i wo
      | i /= wIx = wo
      | otherwise = wo { Mesocycle.exercises = mapWithIndex updateExercise (Mesocycle.exercises wo) }

    updateExercise j ex
      | j /= eIx = ex
      | otherwise = ex { Mesocycle.performedSets = Just ls
                       , Mesocycle.performedReps = Just lr }

    mapWithIndex f = zipWith f [0..]

-- Set-level logging: update a single set's performance (weight & reps)
applySetLog :: SetLogRequest -> Mesocycle.Mesocycle -> Mesocycle.Mesocycle
applySetLog SetLogRequest
  { setWeek = sw
  , setWorkoutIndex = swIx
  , setExerciseIndex = seIx
  , setIndex = sIdx
  , loggedWeight = lw
  , setLoggedReps = setLR
  } m =
  m { Mesocycle.weeks = map updateWeek (Mesocycle.weeks m) }
  where
    updateWeek w
      | Mesocycle.weekNumber w /= sw = w
      | otherwise = w { Mesocycle.workouts = mapWithIndex updateWorkout (Mesocycle.workouts w) }

    updateWorkout i wo
      | i /= swIx = wo
      | otherwise = wo { Mesocycle.exercises = mapWithIndex updateExercise (Mesocycle.exercises wo) }

    updateExercise j ex
      | j /= seIx = ex
      | otherwise =
          let
            upd k sp
              | k == sIdx = sp { Mesocycle.weight = Just lw
                               , Mesocycle.reps   = Just setLR }
              | otherwise = sp
            newSets = zipWith upd [0..] (Mesocycle.setPerformances ex)
            performedSets' = Just (length (filter (\sp -> Mesocycle.weight sp /= Nothing && Mesocycle.reps sp /= Nothing) newSets))
          in ex { Mesocycle.setPerformances = newSets
                , Mesocycle.performedSets   = performedSets'
                }

    mapWithIndex f = zipWith f [0..]

-- Persistence file paths
dataDir :: FilePath
dataDir = "data"


persistFile :: FilePath
persistFile = dataDir ++ "/mesocycle.json"
