# Architecture Overview

This document captures the current state of the codebase and the planned direction for upcoming refactors (Phase 0 baseline). It will evolve as refactors proceed.

## Current Layout (As-Is)

Directories / modules:
- `src/WorkoutTemplate.hs` – static workout template types + YAML parsing.
- `src/Mesocycle.hs` – domain types (Mesocycle, Week, Workout, Exercise, SetPerformance) plus helper logic (findNextActiveExercise, prescribedRIR, etc.).
- `src/MesocyclePersistence.hs` – JSON persistence helpers for Mesocycle.
- `src/Api/Types.hs` – DTOs (VersionResponse, PlanDTO, log requests) and converters from domain.
- `src/Api/Routes.hs` – Servant API type definitions.
- `src/Api/Server.hs` – Assembles application: global IORef state (via unsafePerformIO), sample template creation, handlers, persistence writes.
- `app/Main.hs` – (Executable entrypoint) currently just runs Warp server via exported `app`.
- `test/` – Hspec test driver (`Main.hs`) with unit tests, plus `IntegrationSpec.hs` and `WorkoutLog*` specs.

Characteristics:
- Global state: `globalPlanRef :: IORef Mesocycle` created with `unsafePerformIO`.
- Persistence: ad-hoc writes inside handlers after every mutation.
- DTO <-> domain mapping is one-way (domain -> PlanDTO).
- Logging endpoints mutate structure using index-based traversal chains.
- Some redundant domain fields (`performedSets`, `performedReps` can be inferred from `setPerformances`).

## Identified Issues

| Area | Issue | Impact |
|------|-------|--------|
| State Management | `unsafePerformIO` global IORef | Harder testing, hidden side-effects |
| Traversals | Repeated manual `mapWithIndex` chains | Boilerplate, risk of off-by-one |
| Index Safety | Raw `Int` indices everywhere | Easy to mix indices, no compile-time checks |
| Derived Fields | `performedSets` / `performedReps` stored | Risk of drift / inconsistency |
| Error Handling | Use of `error` for out-of-range | Potential runtime crashes |
| Layering | Handlers construct sample template and persist directly | Blurred responsibilities |
| Naming | Internal field renames to dodge JSON collisions | Cognitive overhead |
| Testing | Manual spec aggregation (improved recently) | Previously risk of forgetting new specs |

## Near-Term Target (Phase 0–2 Vision)

Introduce clear layers:
1. Domain (pure): Mesocycle types, smart constructors, pure update functions (no IO).
2. Repository (IO): load/save abstractions.
3. Application service: orchestrates domain updates + persistence.
4. API layer: Servant types + DTO translation.
5. Frontend (Elm) consumes stable JSON contract.

Key design moves:
- Replace global IORef with `AppEnv` passed via `ReaderT` (or `ReaderT + IORef/TVar`).
- Add newtypes `WeekNumber`, `WorkoutIx`, `ExerciseIx`, `SetIx`, `Reps`, `Weight` for clarity.
- Centralize path navigation: a single function to locate & update an exercise / set by path.
- Remove stored performedSets/performedReps; compute on demand (or keep but ensure invariant enforced in one constructor path).
- Replace partial functions / `error` with `Either DomainError a` or total functions.
- Use generic deriving with Aeson options for DTOs to eliminate manual JSON code where possible.

## Refactor Phases (High-Level)

Phase 0 (documentation & hygiene):
- This document + hlint config (added `HLINT.yaml`).
- Optional: small comment headers per module stating purpose.

Phase 1 (type safety):
- Introduce newtypes for indices & units; adapt domain & DTO mapping.

Phase 2 (state & layering):
- Implement `AppEnv` & `AppM` monad; hoist Servant server; remove `unsafePerformIO`.

Phase 3 (repository abstraction):
- Define `MesocycleRepo` typeclass; extract file persistence implementation.

Phase 4 (update logic consolidation):
- Implement path‑based update helpers; eliminate duplicate mapWithIndex chains.

Phase 5 (domain simplification):
- Remove redundant performed* storage or ensure derived invariants; adjust DTOs.

Phase 6 (error handling):
- Introduce `DomainError`; propagate errors to API (return 400 with message for invalid indices).

Phase 7+ (tests & polish):
- Property tests, hspec-discover, optional performance tweaks (Vectors), README updates.

## Invariants To Make Explicit Later
- Weeks list length == `numWeeks` field (or remove `numWeeks` and derive).
- Each Workout’s exercises length equals initial set counts for every exercise (or validated separately).
- Indices used in logging must be in range.
- `setPerformances` length == `prescribedSets`.

## Glossary
- Mesocycle: Multi-week training plan.
- Week / Workout / Exercise: Hierarchical nesting levels.
- SetPerformance: (weight, reps) optional pair per set.
- DTO: Data Transfer Object for external API representation (frontend-facing).

## Open Questions (to decide before Phase 2)
- Should performedReps represent last set, average, or sum? (Clarify semantics; currently simple copy.)
- Should deload logic be parametrized (config) rather than hard-coded (RIR progression)?
- Do we need historical logs beyond current mesocycle snapshot? (If yes, design a log append-only structure.)

---
This file will be updated at the completion of each phase to reflect the new structure and decisions.


