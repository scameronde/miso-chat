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
      in mapView HandleTimeAction (Time.view tm)

    Model {modelURI=_, modelTime=Nothing, modelCounter=Just cm} ->
      let (View rcaction) = Counter.view cm
      in fmap HandleCounterAction (Counter.view cm)

    _ ->
      viewHome model

mapViewAction :: (a -> b) -> ((a -> c) -> d) -> ((b -> c) -> d)
mapViewAction converter va = va . ( . converter)

mapView :: (a -> b) -> View a -> View b
mapView converter (View ra) = View (mapViewAction converter ra) 


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
  case action of
    NoOp ->
      noEff model

    SwitchToTime ->
      noEff (model { modelTime = Just Time.initialModel })

    SwitchToCounter ->
      noEff (model { modelCounter = Just Counter.initialModel })

    HandleTimeAction Time.Back ->
      noEff (model { modelTime = Nothing })

    HandleCounterAction Counter.Back ->
      noEff (model { modelCounter = Nothing })

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


views :: (Home.Model -> View Home.Action)
views = Home.view
#endif


-- ROUTES

type ClientRoutes
   = View Home.Action


-- HELPERS

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))

