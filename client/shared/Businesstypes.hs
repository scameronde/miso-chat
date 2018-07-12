{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Businesstypes
  ( Id(..)
  , Participant(..)
  , ChatRoom(..)
  , ChatRegistration(..)
  , ChatMessage(..)
  , ChatMessageLog(..)
  , ChatCommand(..)
  ) where

import Data.Aeson
import GHC.Generics
import Miso.String


-- BUSINESS TYPES

newtype Id = Id MisoString deriving (Show, Eq, Generic)
instance ToJSON Id
instance FromJSON Id

data Participant = Participant
  { pid :: Id
  , name :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON Participant
instance FromJSON Participant


data ChatRoom = ChatRoom
  { rid :: Id
  , title :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRoom
instance FromJSON ChatRoom


data ChatRegistration = ChatRegistration
  { participant :: Participant
  , chatRoom :: ChatRoom
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRegistration
instance FromJSON ChatRegistration


data ChatMessage = ChatMessage
  { message :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessage
instance FromJSON ChatMessage


data ChatMessageLog = ChatMessageLog
  { messageLog :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessageLog
instance FromJSON ChatMessageLog


data ChatCommand = Register ChatRegistration | NewMessage ChatMessage deriving (Show, Eq, Generic)
instance ToJSON ChatCommand
instance FromJSON ChatCommand


