{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ChatRoom
  (
    Model
  , Action(GetChatHistory, HandleWebSocket)
  , Field
  , ChatRoom.view
  , ChatRoom.update
  , ChatRoom.subscriptions
  , ChatRoom.initialModel
  ) where

import           Miso          hiding (action_, model)
import           Miso.String

import qualified Businesstypes as BT
import           RestClient


-- MODELS

data Model = Model
    { participant :: BT.Participant
    , chatRoom    :: BT.ChatRoom
    , message     :: MisoString
    , messageLog  :: MisoString
    , errorMsg    :: MisoString
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
            putStrLn "Getting History"
            resOrErr <- getChatHistory (BT.rid (chatRoom model))
            case resOrErr of
              Left err -> return (GetChatHistoryError (ms $ show err))
              Right (BT.ChatMessageLog hist) -> return (GetChatHistorySuccess hist)

        GetChatHistorySuccess messageLog ->
          noEff (model {messageLog = messageLog})

        GetChatHistoryError err ->
          noEff (model {messageLog = err})

        NoOp ->
          noEff model


-- SUBSCRIPTIONS

subscriptions :: [ Sub Action ]
subscriptions = [ websocketSub (URL "ws://localhost:4567/chat") (Protocols []) HandleWebSocket ]

