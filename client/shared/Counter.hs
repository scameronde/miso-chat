{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Counter
  ( 
    Model
  , Action(Back)
  , view
  , initialModel
#ifdef __GHCJS__
  , update
#endif
  ) where

import Data.Aeson
import GHC.Generics
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

-- MODELS

data Model = Model
  {
    modelLastMessage :: Maybe MisoString
  } deriving (Show, Eq)

initialModel :: Model
initialModel = Model { modelLastMessage = Nothing }


-- ACTIONS

data Action
  = NoOp
  | Back
  | HandleWebSocket (WebSocket Message)
  deriving (Eq, Show)


-- VIEWS

view :: Model -> View Action
view m =
  div_
    []
    [ h1_ [] [text "Counter"]
    , span_ [] [text (fromMaybe "No message received" (modelLastMessage m))]
    , button_ [onClick Back] ["Back"]
    ]


-- UPDATE

#ifdef __GHCJS__
update :: Action -> Model -> Effect Action Model
update action model =
  case action of
    HandleWebSocket WebSocketOpen ->
      model <# do
                 putStrLn "web socket open"
                 return NoOp

    HandleWebSocket (WebSocketClose {}) ->
      model <# do
                 putStrLn "web socket close"
                 return NoOp

    HandleWebSocket (WebSocketError {}) ->
      model <# do
                 putStrLn "web socket error"
                 return NoOp

    HandleWebSocket (WebSocketMessage (Message msg)) ->
      noEff (model { modelLastMessage = Just msg })

    Back ->
      noEff model

#endif


