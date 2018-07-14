{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module LoginC
  ( module Login
  , updateLogin
  ) where

import Miso
import Miso.String
import Data.Maybe
import Data.Proxy
import Servant.API
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient(runClientMOrigin)

import Login
import Businesstypes


-- UPDATE

updateLogin :: LoginAction -> LoginModel -> Effect LoginAction LoginModel
updateLogin action model =
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
          let nextAction = case resOrErr of
                             Left err ->
                               ShowError (ms (show err))
                             Right res ->
                               Login res
          return nextAction

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

loginREST :: Client ClientM LoginAPI
loginREST = client (Proxy @LoginAPI)

chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

login pn = runClientMOrigin (loginREST (fromMisoString pn)) chatServer

