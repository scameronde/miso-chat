{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ChatClient
  ( Model
  , Action(NoOp)
  , ChatClient.view
  , ChatClient.update
  , ChatClient.subscriptions
  , ChatClient.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )

import qualified Chat                          as C
import qualified Login                         as L
import qualified NavBar                        as NB

-- MODELS

data Model
    = LoginModel L.Model
    | ChatModel C.Model
    deriving (Show, Eq)


initialModel :: Model
initialModel = LoginModel L.initialModel


-- ACTIONS

data Action
    = LoginAction L.Action
    | ChatAction C.Action
    | NoOp
    deriving (Show, Eq)


-- VIEWS

viewMainArea :: Model -> View Action
viewMainArea model = case model of
  LoginModel model_ -> fmap LoginAction (L.view model_)

  ChatModel  model_ -> fmap ChatAction (C.view model_)


view :: Model -> View Action
view model = div_
  []
  [ NB.viewNavBar model
  , NB.viewMain [div_ [class_ "view-area"] [viewMainArea model]]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model = case (action, model) of
  (LoginAction (L.Login participant), LoginModel _) ->
    (ChatModel (C.initialModel participant)) <# do
      return (ChatAction C.Init)

  (LoginAction action_, LoginModel model_) ->
    mapEff L.update action_ model_ LoginModel LoginAction

  (ChatAction action_, ChatModel model_) ->
    mapEff C.update action_ model_ ChatModel ChatAction

  _ -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = fmap (mapSub ChatAction) C.subscriptions

mapEff
  :: (sa -> sm -> Effect sa sm)
  -> sa
  -> sm
  -> (sm -> m)
  -> (sa -> a)
  -> Effect a m
mapEff updater action model modelWrapper actionWrapper =
  let (Effect rm ra) = updater action model
      newModel       = modelWrapper rm
      newAction      = fmap (mapSub actionWrapper) ra
  in  Effect newModel newAction
