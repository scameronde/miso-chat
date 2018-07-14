{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shared
  ( Model(..)
  , initialModel
  , Action(..)
  , ClientRoutes
  , GetTimeAPI
  , Time(..)
  , Message(..)
  , viewHome
  , viewTime
  , getURI
  , module Businesstypes
  ) where

import Data.Maybe
import Data.Proxy
import Miso
import Miso.String
import Servant.API
import Servant.Utils.Links
import Data.Text (Text)

import Businesstypes
import Login


-- BUSINESS TYPES

#ifndef __GHCJS__
data WebSocket action = WebSocket deriving Show
#endif

-- REST API

type GetTimeAPI = "api" :> "time" :> QueryParam "param" Text :> Get '[JSON] Time

-- MODELS

data Model = Model
  { modelURI :: URI
  , modelLastMessage :: Maybe MisoString
  , modelTime :: Maybe MisoString
  } deriving (Show, Eq)

initialModel :: URI -> Model
initialModel u = Model {modelURI = u, modelLastMessage = Nothing, modelTime = Nothing}

-- ACTIONS

data Action
  = NoOp
  | ChangeURI URI
  | HandleURI URI
  | SetTime MisoString
  | RefreshTime
  | HandleWebSocket (WebSocket Message)
  deriving Show

-- VIEWS

viewHome :: Model -> View Action
viewHome m =
  div_
    []
    [ h1_ [] [text "Home sweet Home"]
    , span_ [] [text (fromMaybe "No message received" (modelLastMessage m))]
    , button_ [onClick (ChangeURI (getURI @("time" :> View Action)))] ["View Time"]
    ]

viewTime :: Model -> View Action
viewTime m =
  div_
    []
    [ h1_ [] [text "Current Time"]
    , div_ [] [text (fromMaybe "Time not available" (modelTime m))]
    , button_ [onClick RefreshTime] ["Refresh Time"]
    , button_ [onClick (ChangeURI (getURI @(View Action)))] ["Go Home"]
    ]

-- ROUTES

type ClientRoutes
   = View Action :<|> ("time" :> View Action)

-- HELPERS

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))

