module Main where

import Test.Hspec
import qualified WorkoutTemplateSpec
import qualified MesocycleSpec
import qualified WorkoutLogOpsSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  WorkoutTemplateSpec.spec
  MesocycleSpec.spec
  WorkoutLogOpsSpec.spec
  IntegrationSpec.spec

