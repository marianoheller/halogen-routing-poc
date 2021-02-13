-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module RoutingPOC.Component.Router where

import Prelude
import Component.HigherOrder.Connect (WithCurrentUser)
import Component.HigherOrder.Connect as Connect
import RoutingPOC.Capability.LogMessages (class LogMessages)
import RoutingPOC.Capability.Navigate (class Navigate, navigate)
import RoutingPOC.Capability.Now (class Now)
import RoutingPOC.Component.Utils (OpaqueSlot)
import RoutingPOC.Data.Profile (Profile)
import RoutingPOC.Data.Route (Route(..), routeCodec)
import RoutingPOC.Env (UserEnv)
import RoutingPOC.Page.Home as Home
import RoutingPOC.Page.Inner as Inner
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash (getHash)

type State
  = { route :: Maybe Route
    , currentUser :: Maybe Profile
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive { | WithCurrentUser () }

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , inner :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , register :: OpaqueSlot Unit
    , settings :: OpaqueSlot Unit
    , profile :: OpaqueSlot Unit
    )

component ::
  forall m r.
  MonadAff m =>
  MonadAsk { userEnv :: UserEnv | r } m =>
  Now m =>
  LogMessages m =>
  Navigate m =>
  H.Component HH.HTML Query {} Void m
component =
  Connect.component
    $ H.mkComponent
        { initialState: \{ currentUser } -> { route: Nothing, currentUser }
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleQuery = handleQuery
                  , handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute
    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ Login, Register ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  -- Display the login page instead of the expected page if there is no current user; a simple
  -- way to restrict access.
  authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing -> -- HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
      html
    Just _ -> html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
      Inner -> HH.slot (SProxy :: _ "inner") unit Inner.component {} absurd
      Login -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
      Register -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
      Settings -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
    Nothing -> HH.div_ [ HH.text "Oh no! That page wasn't found." ]
