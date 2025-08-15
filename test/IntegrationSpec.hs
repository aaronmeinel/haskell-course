module IntegrationSpec (spec) where

import Test.Hspec
-- Minimal imports; relying on servant-client against an in-process Warp server
import Api.Server (mkApp)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
import Servant
import Api.Routes
import Api.Types

-- Reuse the Servant client machinery for integration.

data Env = Env { getVersion :: ClientM VersionResponse
               , getPlan :: ClientM PlanDTO
               , postSetLog :: SetLogRequest -> ClientM LogResponse }

mkEnv :: Env
mkEnv = let (v :<|> p :<|> _ :<|> sl) = client (Proxy :: Proxy RootAPI) in Env v p sl

spec :: Spec
spec = describe "Integration" $ do
  it "serves version and plan and accepts a set log" $ 
    testWithApplication mkApp $ \port -> do
      let testBaseUrl = BaseUrl Http "127.0.0.1" port ""
      mgr <- newManager defaultManagerSettings
      let run c = runClientM c (mkClientEnv mgr testBaseUrl)
      -- Version
      Right (VersionResponse 2) <- run (getVersion mkEnv)
      -- Plan
      Right plan <- run (getPlan mkEnv)
      numWeeks plan `shouldSatisfy` (> 0)
      -- Post a set log to first exercise first set
      let sreq = SetLogRequest { setWeek = 1, setWorkoutIndex = 0, setExerciseIndex = 0, setIndex = 0, loggedWeight = 100.0, setLoggedReps = 10 }
      Right (LogResponse True _) <- run (postSetLog mkEnv sreq)
      -- Fetch plan again and ensure weight/reps stored
      Right plan2 <- run (getPlan mkEnv)
      let firstWeek = head (weeks plan2)
          firstWorkout = head (workouts firstWeek)
          firstExercise = head (exercises firstWorkout)
          firstSet = head (sets firstExercise)
      weight firstSet `shouldBe` Just 100.0
      reps firstSet `shouldBe` Just 10
      pure ()
