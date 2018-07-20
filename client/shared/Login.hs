{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Login
  (
    Model
  , Action(Login)
  , Field
  , Login.view
  , Login.update
  , Login.initialModel
  ) where

import           Miso          hiding (action_, model)
import           Miso.String

import qualified Businesstypes as BT
import           RestClient


-- MODELS

data Model = Model
  { loginName  :: MisoString
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
    div_ [ class_ "alert alert-danger" ] [ text message ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model =
  case action of
    ChangeField Name name ->
      noEff ( model { loginName = name
                    , loginError = clearErrorIfEmpty name (loginError model)
                    })

    ShowError error_ ->
      noEff (model { loginError = error_ })

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
    Login _ ->
      noEff model


clearErrorIfEmpty :: MisoString -> MisoString -> MisoString
clearErrorIfEmpty name error_ =
  if (name == "") then
    ""
  else
    error_


-- UTILS

noError :: Model -> Bool
noError model =
  (loginError model) == ""


noName :: Model -> Bool
noName model =
  (loginName model) == ""

