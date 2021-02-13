-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module RoutingPOC.Page.Inner where

import Prelude
import Component.HigherOrder.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import RoutingPOC.Capability.Navigate (class Navigate, navigate)
import RoutingPOC.Data.Profile (Profile)
import RoutingPOC.Data.Route (Route(..))
import RoutingPOC.Env (UserEnv)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | NavigateTo Route

type State
  = { currentUser :: Maybe Profile
    }

component ::
  forall q o m r.
  MonadAff m =>
  MonadAsk { userEnv :: UserEnv | r } m =>
  Navigate m =>
  H.Component HH.HTML q {} o m
component =
  Connect.component
    $ H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  initialState { currentUser } =
    { currentUser
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state.currentUser of
        Nothing -> pure unit
        profile -> pure unit
    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }
    NavigateTo route -> navigate route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{ currentUser } =
    HH.div_
      [ HH.h2_ [ HH.text "Inner page" ]
      , HH.ul_
          [ HH.button [ HE.onClick \_ -> Just $ NavigateTo Home ] [ HH.text "Go to home page" ]
          ]
      ]
