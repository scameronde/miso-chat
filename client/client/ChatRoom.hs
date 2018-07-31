{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ChatRoom
  ( Model
  , Action(GetChatHistory, HandleWebSocket)
  , Field
  , ChatRoom.view
  , ChatRoom.update
  , ChatRoom.subscriptions
  , ChatRoom.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )
import           Miso.String

import qualified Businesstypes.Participant as Participant
import qualified Businesstypes.ChatRoom as ChatRoom
import qualified Businesstypes.ChatMessage as ChatMessage
import qualified Businesstypes.ChatMessageLog as ChatMessageLog
import qualified Businesstypes.ChatRegistration as ChatRegistration
import qualified Businesstypes.ChatCommand as ChatCommand

import           RestClient


-- MODELS

data Model = Model
    { participant :: Participant.Participant
    , chatRoom    :: ChatRoom.ChatRoom
    , message     :: MisoString
    , messageLog  :: MisoString
    , errorMsg    :: MisoString
    } deriving (Show, Eq)


initialModel :: Participant.Participant -> ChatRoom.ChatRoom -> Model
initialModel participant_ chatRoom_ = Model
  { participant = participant_
  , chatRoom    = chatRoom_
  , message     = ""
  , messageLog  = ""
  , errorMsg    = ""
  }


-- ACTIONS

data Field = NewMessage deriving (Show, Eq)

data Action
    = GetChatHistory
    | GetChatHistorySuccess MisoString
    | GetChatHistoryError MisoString
    | ChangeField Field MisoString
    | SendMessage
    | HandleWebSocket (WebSocket ChatMessage.ChatMessage)
    | NoOp
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model = div_
  [class_ "row"]
  [ h2_ [] [text (ChatRoom.title (chatRoom model))]
  , textarea_
    [ class_ "col-md-12"
    , rows_ "20"
    , style_ $ ("width" =: "100%")
    , value_ (messageLog model)
    ]
    []
  , form_
    [class_ "form-inline", onSubmit SendMessage]
    [ input_
      [ type_ "text"
      , class_ "form-control"
      , size_ "30"
      , placeholder_ (append (Participant.name (participant model)) ": Enter message")
      , value_ (message model)
      , onInput (ChangeField NewMessage)
      ]
    , button_ [class_ "btn btn-primary"] [text "Send"]
    ]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model = case msg of
  ChangeField NewMessage message_ -> noEff (model { message = message_ })

  SendMessage                     -> (model { message = "" }) <# do
    send (ChatCommand.NewMessage (ChatMessage.ChatMessage (message model)))
    return NoOp

  HandleWebSocket (WebSocketMessage msg_) ->
    (model { messageLog = (append (messageLog model) (ChatMessage.message msg_)) })
      <# do
           putStrLn "Message received"
           return (NoOp)

  HandleWebSocket _ -> noEff model

  GetChatHistory    -> model <# do
    send
      (ChatCommand.Register (ChatRegistration.ChatRegistration (participant model) (chatRoom model)))
    putStrLn "Getting History"
    resOrErr <- getChatHistory (ChatRoom.id (chatRoom model))
    case resOrErr of
      Left err -> return (GetChatHistoryError (ms $ show err))
      Right (ChatMessageLog.ChatMessageLog hist) -> return (GetChatHistorySuccess hist)

  GetChatHistorySuccess messageLog_ ->
    noEff (model { messageLog = messageLog_ })

  GetChatHistoryError err_ -> noEff (model { messageLog = err_ })

  NoOp                     -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions =
  [websocketSub (URL "ws://localhost:4567/chat") (Protocols []) HandleWebSocket]

