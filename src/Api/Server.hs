{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Server (mkApp) where

import Servant
import Network.Wai ()
import Network.Wai.Middleware.Cors (simpleCors)
import Api.Routes
import Api.Types ( LogResponse(..)
                 , VersionResponse(..)
                 , fromDomainPlan
                 )
import qualified Mesocycle
import qualified MesocycleService
import Data.IORef
import Servant.Server.StaticFiles ()
import Control.Monad.IO.Class (liftIO)
import qualified MesocyclePersistence


-- Environment holding mutable plan state
data Env = Env { planRef :: IORef Mesocycle.Mesocycle }

initialPlan :: Mesocycle.Mesocycle
initialPlan = MesocycleService.initialPlan

-- Build an Application (loads or initializes persisted plan each time it's called)
mkApp :: IO Application
mkApp = do
  plan <- MesocyclePersistence.loadOrInitMesocycle initialPlan
  ref <- newIORef plan
  let env = Env ref
  pure $ simpleCors $ serve (Proxy :: Proxy FullAPI) (server env)

type FullAPI = RootAPI :<|> Raw

serverRoot :: Env -> Server RootAPI
serverRoot env = versionH :<|> planH :<|> logH :<|> logSetH
  where
    -- API version bumped to 2 after removing duplicated performed fields and changing DTO semantics.
    versionH = pure (VersionResponse 2)
    planH = do
      p <- liftIO (readIORef (planRef env))
      pure (fromDomainPlan p)
    logH reqBody = do
      newPlan <- liftIO $ atomicModifyIORef' (planRef env) (\p -> let p' = MesocycleService.applyLog reqBody p in (p', p'))
      liftIO $ MesocyclePersistence.saveMesocycle MesocyclePersistence.mesocycleFile newPlan
      pure (LogResponse True "Logged")
    logSetH sreq = do
      newPlan <- liftIO $ atomicModifyIORef' (planRef env) (\p -> let p' = MesocycleService.applySetLog sreq p in (p', p'))
      liftIO $ MesocyclePersistence.saveMesocycle MesocyclePersistence.mesocycleFile newPlan
      pure (LogResponse True "Set Logged")

server :: Env -> Server FullAPI
server env = serverRoot env :<|> serveDirectoryFileServer "dist"

-- (applyLog / applySetLog moved to MesocycleService for purity & reuse)

-- (Persistence file path details now centralized in MesocyclePersistence)
