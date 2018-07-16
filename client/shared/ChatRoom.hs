{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Chat
  (
    API
  , Model
  , Action
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

import Businesstypes


-- REST API

-- MODELS

-- ACTIONS

-- VIEWS

#ifdef __GHCJS__
-- UPDATE

-- REST-CLIENT
#endif

-- UTILS

