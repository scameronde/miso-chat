{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ChatRoom
  (
    Model
  , Action
  , Field
  , ChatRoom.view
#ifdef __GHCJS__
  , ChatRoom.update
#endif
  , ChatRoom.initialModel
  ) where

import Data.Proxy
import Miso
import Miso.String
import Data.Text (Text)
import Servant.API
#ifdef __GHCJS__
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient(runClientMOrigin)
#endif

import qualified Businesstypes as BT




-- REST API

type GetChatHistoryAPI = "chatRoom" :> Capture "rid" String :> Get '[JSON] BT.ChatMessageLog

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--    WebSocket.listen "ws://localhost:4567/chat" ReceivedMessage


-- MODELS

data Model = Model
    { participant :: BT.Participant
    , chatRoom :: BT.ChatRoom
    , message :: MisoString
    , messageLog :: MisoString
    , errorMsg :: MisoString
    }


initialModel :: BT.Participant -> BT.ChatRoom -> Model
initialModel participant chatRoom =
    Model { participant = participant
    , chatRoom = chatRoom
    , message = ""
    , messageLog = ""
    , errorMsg = ""
    }
--        ! [ WebSocketClient.sendRegistration (ChatRegistration participant chatRoom)
--          , RestClient.getChatRoom chatRoom.id SetChatHistory
--          ]


-- ACTIONS

data Field = NewMessage

data Action
    = GetChatHistory BT.Id
    | GetChatHistorySuccess MisoString
    | GetChatHistoryError MisoString
    | ChangeField Field MisoString
    | SendMessage MisoString
    | ReceivedMessage MisoString


-- VIEWS

view :: Model -> View Action
view model =
    div_ [ class_ "row" ]
        [ h2_ [] [ text (BT.title (chatRoom model)) ]
        , textarea_ [ class_ "col-md-12", rows_ "20", style_ $ ("width" =: "100%"), value_ (messageLog model) ] []
        , form_ [ class_ "form-inline", onSubmit (SendMessage (message model)) ]
            [ input_
                [ type_ "text"
                , class_ "form-control"
                , size_ "30"
                , placeholder_ (append (BT.name (participant model)) ": Enter message")
                , value_ (message model)
                , onInput (ChangeField NewMessage)
                ]
            , button_ [ class_ "btn btn-primary" ] [ text "Send" ]
            ]
        ]


#ifdef __GHCJS__
-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model =
    case msg of
        ChangeField NewMessage message ->
          noEff model
--            ( model |> messageLens.set message, Cmd.none )

        SendMessage message ->
          noEff model
--            ( model |> messageLens.set "", WebSocketClient.sendMessage (Message message) )

        ReceivedMessage message ->
          noEff model
--            ( model |> messageLogLens.set (model.messageLog ++ message), Cmd.none )

        GetChatHistory id ->
          noEff model

        GetChatHistorySuccess messageLog ->
          noEff model
--            ( model |> messageLogLens.set messageLog.messageLog, Cmd.none )

        GetChatHistoryError err ->
          noEff model
--            ( model |> errorLens.set (toString error), Cmd.none )


initHistory :: Model -> Effect Action Model
initHistory model =
  model <# do
    resOrErr <- getChatHistory (BT.rid (chatRoom model))
    case resOrErr of
      Left err -> return (GetChatHistoryError (ms $ show err))
      Right (BT.ChatMessageLog mlog) -> return (GetChatHistorySuccess mlog)
 

-- REST-CLIENT

chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

getChatHistoryREST :: Client ClientM GetChatHistoryAPI
getChatHistoryREST = client (Proxy @GetChatHistoryAPI)

getChatHistory (BT.Id rid) = runClientMOrigin (getChatHistoryREST (show rid)) chatServer



#endif

-- UTILS

