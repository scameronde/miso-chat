{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatMessageLog where

import           Data.Aeson
import           GHC.Generics
import           Miso.String             hiding ( toLower
                                                , toUpper
                                                , stripPrefix
                                                )


newtype ChatMessageLog = ChatMessageLog
  { messageLog :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessageLog
instance FromJSON ChatMessageLog
