{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Time
  (
    Model
  , Action(Back)
  , view
  , initialModel
#ifdef __GHCJS__
  , update
#endif
  , GetTimeAPI
  ) where

import Data.Maybe
import Data.Proxy
import Miso
import Miso.String
import Servant.API
import Servant.Utils.Links
import Data.Text (Text)

import Businesstypes


-- BUSINESS TYPES

-- REST API

type GetTimeAPI = "api" :> "time" :> Get '[JSON] Time


-- MODELS

data Model = Model
  { 
    modelTime :: Maybe MisoString
  } deriving (Show, Eq)

initialModel :: Model
initialModel = Model { modelTime = Nothing }


-- ACTIONS

data Action
  = SetTime MisoString
  | RefreshTime
  | Back
  deriving (Eq, Show)


-- VIEWS

view :: Model -> View Action
view m =
  div_
    []
    [ h1_ [] [text "Current Time"]
    , div_ [] [text (fromMaybe "Time not available" (modelTime m))]
    , button_ [onClick RefreshTime] ["Refresh Time"]
    , button_ [onClick Back] ["Go Home"]
    ]


-- UPDATE

#ifdef __GHCJS__
update :: Action -> Model -> Effect Action Model
update action model =
  case action of
    SetTime time ->
      noEff (model { modelTime = Just time })

    RefreshTime ->
      model <# do
        resOrError <- getTime
        let nextAction = case resOrError of
                           Left err ->
                             SetTime (ms (show err))
                           Right (Time time) ->
                             SetTime (ms (formatTime defaultTimeLocale "%H:%M:%S" time))
        return nextAction

    Back ->
      noEff model


-- REST API

getTimeREST :: Client ClientM GetTimeAPI
getTimeREST = client (Proxy @GetTimeAPI)

getTime = runClientM (getTimeREST)
#endif

