{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Home
  (
    Model
  , Action
  , Home.view
  , initialModel
#ifdef __GHCJS__
  , Home.update
  , views
#endif
  , ClientRoutes
  , getURI
  ) where

import Data.Maybe
import Data.Proxy
import Miso
import Miso.String
import Servant.API
import Servant.Utils.Links
import Data.Text (Text)

import Businesstypes
import qualified Time
import qualified Counter


-- MODELS

data Model = Model
  { modelURI :: URI
  , modelTime :: Maybe Time.Model
  , modelCounter :: Maybe Counter.Model
  } deriving (Show, Eq)

initialModel :: URI -> Model
initialModel u = Model { modelURI = u, modelTime = Nothing, modelCounter = Nothing }


-- ACTIONS

data Action
  = NoOp
  | ChangeURI URI
  | HandleURI URI
  | HandleTimeAction Time.Action
  | HandleCounterAction Counter.Action
  deriving (Eq, Show)


-- VIEWS

view :: Model -> View Action
view model =
  div_
    []
    [ h1_ [] [text "Home sweet Home"]
    , button_ [onClick (ChangeURI (getURI @("time" :> View Time.Action)))] ["View Time"]
    , button_ [onClick (ChangeURI (getURI @("counter" :> View Counter.Action)))] ["View Counter"]
    ]


-- UPDATE

#ifdef __GHCJS__
update :: Action -> Model -> Effect Action Model
update action model =
  case action of
    NoOp ->
      noEff model

    HandleTimeAction Time.Back ->
      (model { modelTime = Nothing }) <# (pure (ChangeURI (getURI @(View Action))))

    HandleCounterAction Counter.Back ->
      (model { modelCounter = Nothing }) <# (pure (ChangeURI (getURI @(View Action))))

    HandleTimeAction ia ->
      let (Effect rm ra) = Time.update ia (fromMaybe Time.initialModel (modelTime model))
          newModel  = model { modelTime = Just rm }
          newAction = fmap (mapSub HandleTimeAction) ra
      in
        Effect newModel newAction 

    HandleCounterAction ia ->
      let (Effect rm ra) = Counter.update ia (fromMaybe Counter.initialModel (modelCounter model))
          newModel  = model { modelCounter = Just rm }
          newAction = fmap (mapSub HandleCounterAction) ra
      in
        Effect newModel newAction 

    HandleURI u ->
      noEff (model { modelURI = u })

    ChangeURI u ->
      model <# (pushURI u *> return NoOp)


views :: (Home.Model -> View Home.Action) :<|> (Time.Model -> View Time.Action) :<|> (Counter.Model -> View Counter.Action)
views = Home.view :<|> Time.view :<|> Counter.view
#endif


-- ROUTES

type ClientRoutes
   = View Home.Action :<|> ("time" :> View Time.Action) :<|> ("counter" :> View Counter.Action)


-- HELPERS

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))

