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
    Model(modelURI)
  , Action(NoOp, HandleCounterAction)
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
  | SwitchToTime
  | SwitchToCounter
  | HandleTimeAction Time.Action
  | HandleCounterAction Counter.Action
  deriving (Eq, Show)


-- VIEWS

view :: Model -> View Action
view model =
  case model of
    Model {modelURI=_, modelTime=Just tm, modelCounter=Nothing} ->
      let (View rtaction) = Time.view tm
      in fmap HandleTimeAction (Time.view tm)

    Model {modelURI=_, modelTime=Nothing, modelCounter=Just cm} ->
      let (View rcaction) = Counter.view cm
      in fmap HandleCounterAction (Counter.view cm)

    _ ->
      viewHome model


viewHome :: Model -> View Action
viewHome model =
  div_
    []
    [ h1_ [] [text "Home sweet Home"]
    , button_ [onClick SwitchToTime] ["View Time"]
    , button_ [onClick SwitchToCounter] ["View Counter"]
    ]


-- UPDATE

#ifdef __GHCJS__
update :: Action -> Model -> Effect Action Model
update action model =
  case (action, model) of
    (NoOp, _) ->
      noEff model

    (SwitchToTime, _) ->
      noEff (model { modelTime = Just Time.initialModel })

    (SwitchToCounter, _) ->
      noEff (model { modelCounter = Just Counter.initialModel })

    (HandleTimeAction Time.Back, _) ->
      noEff (model { modelTime = Nothing })

    (HandleCounterAction Counter.Back, _) ->
      noEff (model { modelCounter = Nothing })

    (HandleTimeAction ta, Model {modelURI=_, modelTime=Just tm, modelCounter=Nothing}) ->
      let (Effect rm ra) = Time.update ta tm
          newModel  = model { modelTime = Just rm }
          newAction = fmap (mapSub HandleTimeAction) ra
      in
        Effect newModel newAction 

    (HandleCounterAction ca, Model {modelURI=_, modelTime=Nothing, modelCounter=Just cm}) ->
      let (Effect rm ra) = Counter.update ca cm
          newModel  = model { modelCounter = Just rm }
          newAction = fmap (mapSub HandleCounterAction) ra
      in
        Effect newModel newAction 

    (_, _) ->
      noEff model


views :: (Home.Model -> View Home.Action)
views = Home.view
#endif


-- ROUTES

type ClientRoutes
   = View Home.Action


-- HELPERS

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))

