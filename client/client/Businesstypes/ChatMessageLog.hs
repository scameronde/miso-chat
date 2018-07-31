{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatMessageLog where

import           Data.Aeson
import           GHC.Generics
import           Miso.String             hiding ( toLower
                                                , toUpper
                                                , stripPrefix
                                                )


data ChatMessageLog = ChatMessageLog
  { messageLog :: MisoString
  } deriving (Show, Eq, Generic)
instance ToJSON ChatMessageLog
instance FromJSON ChatMessageLog
