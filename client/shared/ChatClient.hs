{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ChatClient
  (
    Model
  , Action(NoOp)
  , ChatClient.view
#ifdef __GHCJS__
  , ChatClient.update
  , ChatClient.subscriptions
#endif
  , ChatClient.initialModel
  ) where

import Data.Proxy
import Miso
import Miso.String
import Data.Text (Text)
import Servant.API
#ifdef __GHCJS__
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient(runClientMOrigin)
#endif

import qualified Businesstypes as BT
import qualified Chat as C
import qualified Login as L
import qualified NavBar as NB


-- REST API

-- MODELS

data Model
    = LoginModel L.Model
    | ChatModel C.Model
    deriving (Show, Eq)


initialModel :: Model
initialModel = LoginModel L.initialModel


-- ACTIONS

data Action
    = HandleLoginAction L.Action
    | HandleChatAction C.Action
    | NoOp
    deriving (Show, Eq)


-- VIEWS

viewMainArea :: Model -> View Action
viewMainArea model =
    case model of
        LoginModel model_ ->
          fmap HandleLoginAction (L.view model_)

        ChatModel model_ ->
          fmap HandleChatAction (C.view model_)


view :: Model -> View Action
view model =
    div_ []
        [ NB.viewNavBar model
        , NB.viewMain [ div_ [ class_ "view-area" ] [ viewMainArea model ] ]
        ]


#ifdef __GHCJS__
-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model =
    case ( msg, model ) of
        ( HandleLoginAction (L.Login participant), LoginModel model_ ) ->
          (ChatModel (C.initialModel participant)) <# do
            return (HandleChatAction C.Init)

        ( HandleLoginAction msg_, LoginModel model_ ) ->
          let (Effect rm ra) = L.update msg_ model_
              newModel = LoginModel rm
              newAction = fmap (mapSub HandleLoginAction) ra
          in
            Effect newModel newAction

        ( HandleChatAction msg_, ChatModel model_ ) ->
          let (Effect rm ra) = C.update msg_ model_
              newModel = ChatModel rm
              newAction = fmap (mapSub HandleChatAction) ra
          in
            Effect newModel newAction

        _ ->
          noEff model


-- REST-CLIENT

subscriptions :: [ Sub Action ]
subscriptions = fmap (mapSub HandleChatAction) C.subscriptions


#endif

-- UTILS

