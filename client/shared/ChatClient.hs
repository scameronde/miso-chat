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

import Miso
#ifdef __GHCJS__
#endif

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
update action model =
    case ( action, model ) of
        ( HandleLoginAction (L.Login participant), LoginModel _ ) ->
          (ChatModel (C.initialModel participant)) <# do
            return (HandleChatAction C.Init)

        ( HandleLoginAction action_, LoginModel model_ ) ->
          let (Effect rm ra) = L.update action_ model_
              newModel = LoginModel rm
              newAction = fmap (mapSub HandleLoginAction) ra
          in
            Effect newModel newAction

        ( HandleChatAction action_, ChatModel model_ ) ->
          let (Effect rm ra) = C.update action_ model_
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

