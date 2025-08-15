{-# LANGUAGE NamedFieldPuns #-}
module MesocycleService (
    initialPlan
  , applyLog
  , applySetLog
) where

import qualified Mesocycle
import qualified WorkoutTemplate
import Api.Types (ExerciseLogRequest(..), SetLogRequest(..))

-- | Initial mesocycle plan used on first launch.
initialPlan :: Mesocycle.Mesocycle
initialPlan = Mesocycle.generateMesocycle sampleTemplate 4
  where
    sampleTemplate = WorkoutTemplate.WorkoutTemplate [ pushDay, pullDay, legsDay ]

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

-- | Apply bulk logging of N sets with the same reps value.
applyLog :: ExerciseLogRequest -> Mesocycle.Mesocycle -> Mesocycle.Mesocycle
applyLog ExerciseLogRequest { week = wk, workoutIndex = wIx, exerciseIndex = eIx, loggedSets = ls, loggedReps = lr } m =
  m { Mesocycle.weeks = map updateWeek (Mesocycle.weeks m) }
  where
    updateWeek w
      | Mesocycle.unWeekNumber (Mesocycle.weekNumber w) /= wk = w
      | otherwise = w { Mesocycle.workouts = mapWithIndex updateWorkout (Mesocycle.workouts w) }

    updateWorkout i wo
      | i /= wIx = wo
      | otherwise = wo { Mesocycle.exercises = mapWithIndex updateExercise (Mesocycle.exercises wo) }

    updateExercise j ex
      | j /= eIx = ex
      | otherwise =
          let upd k sp
                | k < ls = case Mesocycle.reps sp of
                              Nothing -> sp { Mesocycle.reps = Just (Mesocycle.Reps lr) }
                              Just _  -> sp
                | otherwise = sp
              newSets = zipWith upd [0..] (Mesocycle.setPerformances ex)
          in ex { Mesocycle.setPerformances = newSets }

    mapWithIndex f = zipWith f [0..]

-- | Apply logging for a single set (weight + reps).
applySetLog :: SetLogRequest -> Mesocycle.Mesocycle -> Mesocycle.Mesocycle
applySetLog SetLogRequest { setWeek = sw, setWorkoutIndex = swIx, setExerciseIndex = seIx, setIndex = sIdx, loggedWeight = lw, setLoggedReps = setLR } m =
  m { Mesocycle.weeks = map updateWeek (Mesocycle.weeks m) }
  where
    updateWeek w
      | Mesocycle.unWeekNumber (Mesocycle.weekNumber w) /= sw = w
      | otherwise = w { Mesocycle.workouts = mapWithIndex updateWorkout (Mesocycle.workouts w) }

    updateWorkout i wo
      | i /= swIx = wo
      | otherwise = wo { Mesocycle.exercises = mapWithIndex updateExercise (Mesocycle.exercises wo) }

    updateExercise j ex
      | j /= seIx = ex
      | otherwise =
          let upd k sp
                | k == sIdx = sp { Mesocycle.weight = Just (Mesocycle.Weight lw)
                                  , Mesocycle.reps   = Just (Mesocycle.Reps setLR) }
                | otherwise = sp
              newSets = zipWith upd [0..] (Mesocycle.setPerformances ex)
          in ex { Mesocycle.setPerformances = newSets }

    mapWithIndex f = zipWith f [0..]
