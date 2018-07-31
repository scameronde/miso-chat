{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Businesstypes.ChatRegistration where

import Businesstypes.Participant
import Businesstypes.ChatRoom
import           Data.Aeson
import           GHC.Generics


data ChatRegistration = ChatRegistration
  { participant :: Participant
  , chatRoom    :: ChatRoom
  } deriving (Show, Eq, Generic)
instance ToJSON ChatRegistration
instance FromJSON ChatRegistration
