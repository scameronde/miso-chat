{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  Login
Description :  A poor man's login.

This module returns with the loged in 'Participant' as payload of the 'Login' action.
-}
module Login
  ( Model
  , Action(Login)
  , Login.desc
--  , Login.view
--  , Login.update
--  , Login.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )
import           Miso.String
import           Module                         ( Module(..) )

import           Businesstypes.Participant      ( Participant )
import           RestClient


-- DESCRIPTION

desc :: Module Model Action
desc = Module
  { _model  = Login.initialModel
  , _action = Login.NoOp
  , _view   = Login.view
  , _update = Login.update
  , _subs   = []
  }


-- MODELS

data Model = Model
  { loginName  :: MisoString
  , loginError :: MisoString
  } deriving (Show, Eq)


initialModel :: Model
initialModel = Model {loginName = "", loginError = ""}


-- ACTIONS

data Field = Name deriving (Show, Eq)

data Action
  = Login Participant
  | GetParticipant
  | ChangeField Field MisoString
  | ShowError MisoString
  | NoOp
  deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model = div_
  []
  [ viewErrorMsg model "Wrong Credentials!"
  , form_
    [onSubmit GetParticipant]
    [ div_
      [class_ "form-group"]
      [ label_ [for_ "nameInput"] [text "Your name"]
      , input_
        [ id_ "nameInput"
        , type_ "text"
        , class_ "form-control"
        , onInput (ChangeField Name)
        ]
      ]
    , button_ [class_ "btn btn-primary", disabled_ (noName model)] [text "OK"]
    ]
  ]


viewErrorMsg :: Model -> MisoString -> View Action
viewErrorMsg model message = 
  if noError model
    then div_ [] []
    else div_ [class_ "alert alert-danger"] [text message]


-- UPDATE

update :: Action -> Model -> Effect Action Model

update (ChangeField Name name) model = 
  noEff model { loginName = name, loginError = clearErrorIfEmpty name (loginError model) }

update (ShowError err) model = 
  noEff (model { loginError = err })

update GetParticipant model =
  if loginName model == ""
    then noEff model
    else model <# do
      resOrErr <- login (loginName model)
      case resOrErr of
        Left  err -> return (ShowError (ms (show err)))
        Right res -> return (Login res)

update _ model =
  noEff model
       

-- UTILS

clearErrorIfEmpty :: MisoString -> MisoString -> MisoString
clearErrorIfEmpty name err = if name == "" then "" else err

noError :: Model -> Bool
noError model = loginError model == ""


noName :: Model -> Bool
noName model = loginName model == ""
