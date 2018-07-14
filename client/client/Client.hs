{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module HomeC
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

import qualified Home
import qualified Counter
import qualified Time


main :: IO ()
main = do
  miso $ \currentURI ->
    App
      { model = Home.initialModel currentURI
      , view = \m -> case runRoute (Proxy @Home.ClientRoutes) Home.views Home.modelURI m of
          Left _ -> Home.view m
          Right v -> v
      , update = Home.update
      , initialAction = Home.NoOp
      , events = defaultEvents
      , subs =
          [ uriSub Home.HandleURI
          , websocketSub
              (URL "ws://localhost:8080/websocket")
              (Protocols [])
              Counter.HandleWebSocket
          ]
      , mountPoint = Nothing
      }

