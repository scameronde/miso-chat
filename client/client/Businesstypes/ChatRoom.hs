{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatRoom where

import Businesstypes.Id

import           Data.Aeson
import           GHC.Generics
import           Miso.String             hiding ( toLower
                                                , toUpper
                                                , stripPrefix
                                                )


data ChatRoom = ChatRoom
  { id   :: Id
  , title :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRoom
instance FromJSON ChatRoom
