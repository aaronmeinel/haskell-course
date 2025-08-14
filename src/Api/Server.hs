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

-- For now we generate a trivial plan each server start.
-- Later: load from file or configuration.

plan :: Mesocycle.Mesocycle
plan = Mesocycle.generateMesocycle sampleTemplate 4
  where
    sampleTemplate = WorkoutTemplate.WorkoutTemplate [] -- Placeholder empty template

server :: Server RootAPI
server = versionH :<|> planH
  where
    versionH = pure (VersionResponse 1)
    planH = pure (fromDomainPlan plan)

app :: Application
app = simpleCors $ serve (Proxy :: Proxy RootAPI) server
