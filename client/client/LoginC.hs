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
          resOrErr <- runClientM (login (fromMisoString (loginName model)))
          return $ case resOrErr of
            Left err ->
              ShowError (ms (show err))
            Right res ->
              Login res

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
 

-- UTILS

login :: Client ClientM LoginAPI
login = client (Proxy @LoginAPI)

