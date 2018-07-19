{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Login
  ( 
    API
  , Model
  , Action(Login)
  , Field
  , Login.view
#ifdef __GHCJS__
  , Login.update
#endif
  , Login.initialModel
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


-- REST API

type API = "participant" :> Capture "participantName" Text :> Get '[JSON] BT.Participant


-- MODELS

data Model = Model
  { loginName :: MisoString
  , loginError :: MisoString
  } deriving (Show, Eq)


initialModel :: Model
initialModel = Model { loginName = "", loginError = "" }


-- ACTIONS

data Field = Name deriving (Show, Eq)

data Action 
  = Login BT.Participant
  | GetParticipant
  | ChangeField Field MisoString
  | ShowError MisoString
  deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model =
  div_ []
       [ viewErrorMsg model "Wrong Credentials!"
       , form_ [ onSubmit GetParticipant ]
               [ div_ [ class_ "form-group" ]
                      [ label_ [ for_ "nameInput" ] [ text "Your name" ]
                      , input_ [ id_ "nameInput", type_ "text", class_ "form-control", onInput (ChangeField Name) ]
                      ]     
               , button_ [ class_ "btn btn-primary", disabled_ (noName model) ] [ text "OK" ]
               ]
       ]
 

viewErrorMsg :: Model -> MisoString -> View Action
viewErrorMsg model message =
  if (noError model) then
    div_ [] []
  else
    div_ [ class_ "alert alert-danger" ] [ text (append (append message " -> ") (loginError model)) ]


-- UPDATE

#ifdef __GHCJS__
update :: Action -> Model -> Effect Action Model
update action model =
  case action of
    ChangeField Name name ->
      noEff ( model { loginName = name
                    , loginError = clearErrorIfEmpty name (loginError model)
                    })

    ShowError error ->
      noEff (model { loginError = error })

    GetParticipant ->
      if (loginName model == "") then
        noEff model
      else
        model <# do
          resOrErr <- login (loginName model)
          case resOrErr of
            Left err ->
              return (ShowError (ms (show err)))
            Right res ->
              return (Login res)

    -- for external communication
    Login participant ->
      noEff model

    _ -> noEff model


clearErrorIfEmpty :: MisoString -> MisoString -> MisoString
clearErrorIfEmpty name error =
  if (name == "") then
    ""
  else
    error
 

-- REST CLIENT

loginREST :: Client ClientM API
loginREST = client (Proxy @API)

chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

login pn = runClientMOrigin (loginREST (fromMisoString pn)) chatServer
#endif


-- UTILS

noError :: Model -> Bool
noError model =
  (loginError model) == ""


noName :: Model -> Bool
noName model =
  (loginName model) == ""

