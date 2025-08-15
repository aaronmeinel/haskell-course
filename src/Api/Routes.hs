{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Api.Routes where

import Servant
import Api.Types
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

-- Version always returns {"apiVersion":1}
-- Plan returns the prescription-only plan

type APIV1 = 
    "version" :> Get '[JSON] VersionResponse
  :<|> "plan"    :> Get '[JSON] PlanDTO
  :<|> "logSet"  :> ReqBody '[JSON] SetLogRequest :> Post '[JSON] LogResponse

-- Wrap version response to allow extension later
newtype VersionResponse = VersionResponse { apiVersion :: Int }
  deriving (Eq, Show, Generic, ToJSON)

-- Top-level API under /api

type RootAPI = "api" :> APIV1
