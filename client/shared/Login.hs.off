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
  ( LoginAPI
  , LoginModel(..)
  , LoginAction(..)
  , LoginField(..)
  , viewLogin
  , initialLoginModel
  ) where

import Miso
import Miso.String
import Data.Text (Text)
import Servant.API

import Businesstypes


-- REST API

type LoginAPI = "participant" :> Capture "participantName" Text :> Get '[JSON] Participant


-- MODELS

data LoginModel = LoginModel
  { loginName :: MisoString
  , loginError :: MisoString
  } deriving (Show, Eq)


initialLoginModel :: LoginModel
initialLoginModel = LoginModel { loginName = "", loginError = "" }


-- ACTIONS

data LoginField = Name

data LoginAction 
  = Login Participant
  | GetParticipant
  | ChangeField LoginField MisoString
  | ShowError MisoString


-- VIEWS

viewLogin :: LoginModel -> View LoginAction
viewLogin model =
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
 

viewErrorMsg :: LoginModel -> MisoString -> View LoginAction
viewErrorMsg model message =
  if (noError model) then
    div_ [] []
  else
    div_ [ class_ "alert alert-danger" ] [ text message ]


-- UTILS

noError :: LoginModel -> Bool
noError model =
  (loginError model) == ""


noName :: LoginModel -> Bool
noName model =
  (loginName model) == ""

