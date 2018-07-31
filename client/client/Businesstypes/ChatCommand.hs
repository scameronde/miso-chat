{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatCommand where

import           Businesstypes.ChatRegistration
import           Businesstypes.ChatMessage
import           Data.Aeson
import           GHC.Generics

data ChatCommand = Register ChatRegistration | NewMessage ChatMessage deriving (Show, Eq, Generic)
instance ToJSON ChatCommand where
  toJSON (Register reg) =
    object [ "command" .= ("register"::String)
           , "registration" .= (toJSON reg)
           ]

  toJSON (NewMessage msg) =
    object [ "command" .= ("message"::String)
           , "chatMessage" .= (toJSON msg)
           ]
