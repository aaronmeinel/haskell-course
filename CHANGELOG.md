# Revision history for haskell-course

## 0.2.0.0 -- 2025-08-15

* API version bumped to 2 (GET /version now returns {"apiVersion":2}).
* Removed duplicated fields performedSets / performedReps from domain model; these are now derived.
* ExerciseDTO changed: performedSets now Int (derived completed set count), performedReps now Maybe Int (last logged reps), no stored duplication.
* Added helper functions: completedSets, exerciseComplete, lastLoggedReps.
* Moved JSON instances for WorkoutLog types to eliminate orphan instances.
* Test suite modularized (Main.hs only orchestrates spec modules).

## 0.1.0.0 -- 2025-08-15

* Initial version.
