module Main where

import Prelude

import Affjax (request, printError)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import RoutingPOC.Api.Endpoint (Endpoint(..))
import RoutingPOC.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, readToken)
import RoutingPOC.AppM (runAppM)
import RoutingPOC.Component.Router as Router
import RoutingPOC.Data.Profile as Profile
import RoutingPOC.Data.Route (routeCodec)
import RoutingPOC.Env (LogLevel(..), Env)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      baseUrl = BaseURL "https://conduit.productionready.io"

      logLevel = Dev
    userEnv <-
      liftEffect do
        currentUser <- Ref.new Nothing
        userBus <- Bus.make
        readToken
          >>= traverse_ \token -> do
              let
                requestOptions = { endpoint: User, method: Get }
              launchAff_ do
                res <- request $ defaultRequest baseUrl (Just token) requestOptions
                let
                  user :: Either String _
                  user = case res of
                    Left e -> Left (printError e)
                    Right v ->
                      lmap printJsonDecodeError do
                        u <- Codec.decode (CAR.object "User" { user: CA.json }) v.body
                        CA.decode Profile.profileCodec u.user
                liftEffect (Ref.write (hush user) currentUser)
        pure { currentUser, userBus }
    let
      environment :: Env
      environment = { baseUrl, logLevel, userEnv }

      rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
      rootComponent = H.hoist (runAppM environment) Router.component
    halogenIO <- runUI rootComponent {} body
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
