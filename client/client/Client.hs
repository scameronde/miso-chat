{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  (
     main
  ) where

import Data.Maybe
import Data.Proxy
import Data.Time.Format
import Miso
import Miso.String
import Servant.API
import Servant.Client.Ghcjs

import qualified ChatClient

main :: IO ()
main = do
  startApp
    App
      { model = ChatClient.initialModel
      , view = ChatClient.view
      , update = ChatClient.update
      , initialAction = ChatClient.NoOp
      , events = defaultEvents
      , subs = ChatClient.subscriptions
      , mountPoint = Nothing
      }

