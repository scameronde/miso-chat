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
  , Action(GetChatHistory, HandleWebSocket)
  , Field
  , ChatRoom.view
#ifdef __GHCJS__
  , ChatRoom.update
  , ChatRoom.subscriptions
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


-- MODELS

data Model = Model
    { participant :: BT.Participant
    , chatRoom :: BT.ChatRoom
    , message :: MisoString
    , messageLog :: MisoString
    , errorMsg :: MisoString
    } deriving (Show, Eq)


initialModel :: BT.Participant -> BT.ChatRoom -> Model
initialModel participant chatRoom =
    Model { participant = participant
    , chatRoom = chatRoom
    , message = ""
    , messageLog = ""
    , errorMsg = ""
    }


-- ACTIONS

data Field = NewMessage deriving (Show, Eq)

data Action
    = GetChatHistory
    | GetChatHistorySuccess MisoString
    | GetChatHistoryError MisoString
    | ChangeField Field MisoString
    | SendMessage
    | HandleWebSocket (WebSocket BT.ChatMessage)
    | NoOp
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model =
    div_ [ class_ "row" ]
        [ h2_ [] [ text (BT.title (chatRoom model)) ]
        , textarea_ [ class_ "col-md-12", rows_ "20", style_ $ ("width" =: "100%"), value_ (messageLog model) ] []
        , form_ [ class_ "form-inline", onSubmit SendMessage ]
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
          noEff (model {message = message})

        SendMessage ->
          (model {message = ""}) <# do
            send (BT.NewMessage (BT.ChatMessage (message model)))
            return NoOp

        HandleWebSocket (WebSocketMessage msg) ->
          (model {messageLog = (append (messageLog model) (BT.chatMessage msg))}) <# do
            putStrLn "Message received"
            return (NoOp)
      
        HandleWebSocket _ ->
          noEff model

        GetChatHistory ->
          model <# do
            send (BT.Register (BT.ChatRegistration (participant model) (chatRoom model)))
            resOrErr <- getChatHistory (BT.rid (chatRoom model))
            case resOrErr of
              Left err -> return (GetChatHistoryError (ms $ show err))
              Right (BT.ChatMessageLog hist) -> return (GetChatHistorySuccess hist)

        GetChatHistorySuccess messageLog ->
          noEff (model {messageLog = messageLog})

        GetChatHistoryError err ->
          noEff (model {errorMsg = err})

        NoOp ->
          noEff model


-- REST-CLIENT

chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

getChatHistoryREST :: Client ClientM GetChatHistoryAPI
getChatHistoryREST = client (Proxy @GetChatHistoryAPI)

getChatHistory (BT.Id rid) = runClientMOrigin (getChatHistoryREST (show rid)) chatServer

subscriptions :: [ Sub Action ]
subscriptions = [ websocketSub (URL "ws://localhost:4567/chat") (Protocols []) HandleWebSocket ]
   
#endif

-- UTILS

