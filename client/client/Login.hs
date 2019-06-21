{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      :  Login
Description :  A poor man's login.

This module returns with the loged in 'Participant' as payload of the 'Login' action.
-}
module Login
  ( Login(..)
  , Action (LoginParticipant)
  , Config (LoginConfig)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , view
                                                , update
                                                )
import           Miso.String
import           Module                         ( Module(..) )

import           Businesstypes.Participant      ( Participant )
import           RestClient


data Login = Login

instance Module Login where

  data Model Login = Model
    { loginName  :: MisoString
    , loginError :: MisoString
    } deriving (Show, Eq)

  data Action Login
    = LoginParticipant Participant
    | GetParticipant
    | ChangeField Field MisoString
    | ShowError MisoString
    | NoOp
    deriving (Show, Eq)

  data Config Login = LoginConfig

  initialModelM = initialModel

  initialActionM = NoOp

  viewM = view

  updateM = update

  subscriptionsM = []


-- MODEL

initialModel :: Config Login -> Model Login
initialModel _ = Model {loginName = "", loginError = ""}


-- ACTION

data Field = Name deriving (Show, Eq)


-- VIEWS

view :: Model Login -> View (Action Login)
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


viewErrorMsg :: Model Login -> MisoString -> View (Action Login)
viewErrorMsg model message = 
  if noError model
    then div_ [] []
    else div_ [class_ "alert alert-danger"] [text message]


-- UPDATE

update :: Action Login -> Model Login -> Effect (Action Login) (Model Login)

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
        Right res -> return (LoginParticipant res)

update _ model =
  noEff model
       

-- UTILS

clearErrorIfEmpty :: MisoString -> MisoString -> MisoString
clearErrorIfEmpty name err = if name == "" then "" else err

noError :: Model Login -> Bool
noError model = loginError model == ""


noName :: Model Login -> Bool
noName model = loginName model == ""
