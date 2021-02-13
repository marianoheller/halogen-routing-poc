-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module RoutingPOC.Page.Home where

import Prelude
import Component.HigherOrder.Connect as Connect
import RoutingPOC.Capability.Navigate (class Navigate)
import RoutingPOC.Data.Profile (Profile)
import RoutingPOC.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }

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

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{ currentUser } =
    HH.div_
      [ HH.text "Home page"
      ]
