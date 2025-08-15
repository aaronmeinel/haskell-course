{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Api.Routes where

import Servant
import Api.Types

-- Version always returns {"apiVersion":1}
-- Plan returns the prescription-only plan

type APIV1 = 
    "version" :> Get '[JSON] VersionResponse
  :<|> "plan"    :> Get '[JSON] PlanDTO
  :<|> "log"     :> ReqBody '[JSON] ExerciseLogRequest :> Post '[JSON] LogResponse
  :<|> "logSet"  :> ReqBody '[JSON] SetLogRequest :> Post '[JSON] LogResponse

-- VersionResponse now provided by Api.Types

-- Top-level API under /api

type RootAPI = "api" :> APIV1
