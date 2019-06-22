{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatMessage where

import           Data.Aeson
import           GHC.Generics
import           Miso.String             hiding ( toLower
                                                , toUpper
                                                , stripPrefix
                                                )


newtype ChatMessage = ChatMessage
  { message :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessage
instance FromJSON ChatMessage
