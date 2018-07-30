{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes
  ( Id(..)
  , Participant(..)
  , ChatRoom(..)
  , ChatRegistration(..)
  , ChatMessage(..)
  , ChatMessageLog(..)
  , ChatCommand(..)
  , Time(..)
  , Message(..)
  )
where

import           Data.Aeson
import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Time.LocalTime
import           GHC.Generics
import           Miso.String             hiding ( toLower
                                                , toUpper
                                                , stripPrefix
                                                )


-- BUSINESS TYPES

newtype Id = Id MisoString deriving (Show, Eq, Generic)
instance ToJSON Id
instance FromJSON Id

data Participant = Participant
  { participantId  :: Id
  , participantName :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON Participant where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = dropPrefix "participant"
  }
instance FromJSON Participant where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = addPrefix "participant"
  }

toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (c : cs) = toLower c : cs

toUpperFirst :: String -> String
toUpperFirst []       = []
toUpperFirst (c : cs) = toUpper c : cs

dropPrefix :: String -> String -> String
dropPrefix prefix fieldName =
  fromMaybe fieldName (fmap toLowerFirst (stripPrefix prefix fieldName))

addPrefix :: String -> String -> String
addPrefix prefix attributeName = prefix ++ (toUpperFirst attributeName)


data ChatRoom = ChatRoom
  { rid   :: Id
  , title :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRoom
instance FromJSON ChatRoom


data ChatRegistration = ChatRegistration
  { participant :: Participant
  , chatRoom    :: ChatRoom
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRegistration
instance FromJSON ChatRegistration


data ChatMessage = ChatMessage
  { chatMessage :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessage
instance FromJSON ChatMessage


data ChatMessageLog = ChatMessageLog
  { chatMessageLog :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessageLog
instance FromJSON ChatMessageLog


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


newtype Time = Time ZonedTime deriving (Show, Generic)
instance ToJSON Time
instance FromJSON Time

newtype Message = Message MisoString deriving (Eq, Show, Generic, Monoid)
instance ToJSON Message
instance FromJSON Message

