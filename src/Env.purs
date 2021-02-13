module RoutingPOC.Env where

import Prelude
import RoutingPOC.Api.Request (BaseURL)
import RoutingPOC.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

type Env
  = { logLevel :: LogLevel
    , baseUrl :: BaseURL
    , userEnv :: UserEnv
    }

data LogLevel
  = Dev
  | Prod

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel

type UserEnv
  = { userBus :: BusRW (Maybe Profile)
    , currentUser :: Ref (Maybe Profile)
    }
