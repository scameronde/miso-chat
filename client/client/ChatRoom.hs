{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  ChatRoom
Description :  Displays the content of one chat room and allows posting messages.

This module uses web sockets to communicate with the server
-}
module ChatRoom
  ( ChatRoomM(..)
  , Config(ChatRoomConfig)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , view
                                                , update
                                                )
import           Miso.String
import           Module                         ( Module(..) )

import           Businesstypes.Participant      ( Participant )
import qualified Businesstypes.Participant     as Participant
import           Businesstypes.ChatRoom         ( ChatRoom ) 
import qualified Businesstypes.ChatRoom        as ChatRoom
import           Businesstypes.ChatMessage      ( ChatMessage(ChatMessage) )
import qualified Businesstypes.ChatMessage     as ChatMessage
import           Businesstypes.ChatMessageLog   ( ChatMessageLog(ChatMessageLog)
                                                )
import           Businesstypes.ChatRegistration ( ChatRegistration
                                                  ( ChatRegistration
                                                  )
                                                )
import           Businesstypes.ChatCommand      ( )
import qualified Businesstypes.ChatCommand     as ChatCommand

import           RestClient


-- MODULE DESCRIPTION

data ChatRoomM = ChatRoomM

instance Module ChatRoomM where

  data Model ChatRoomM = Model
    { participant :: Participant
    , chatRoom    :: ChatRoom
    , message     :: MisoString
    , messageLog  :: MisoString
    , errorMsg    :: MisoString
    } deriving (Show, Eq)

  data Action ChatRoomM
    = GetChatHistory
    | GetChatHistorySuccess MisoString
    | GetChatHistoryError MisoString
    | ChangeField Field MisoString
    | SendMessage
    | HandleWebSocket (WebSocket ChatMessage)
    | NoOp
    deriving (Show, Eq)

  data Config ChatRoomM = ChatRoomConfig Participant ChatRoom

  initialModelM = initialModel

  initialActionM = GetChatHistory

  viewM = view

  updateM = update

  subscriptionsM = subscriptions


  -- MODELS

initialModel :: Config ChatRoomM -> Model ChatRoomM
initialModel (ChatRoomConfig participant_ chatRoom_) = Model
  { participant = participant_
  , chatRoom    = chatRoom_
  , message     = ""
  , messageLog  = ""
  , errorMsg    = ""
  }


-- ACTIONS

data Field = NewMessage deriving (Show, Eq)


-- VIEWS

view :: Model ChatRoomM -> View (Action ChatRoomM)
view model = div_
  [class_ "row"]
  [ h2_ [] [text (ChatRoom.title (chatRoom model))]
  , textarea_
    [ class_ "col-md-12"
    , rows_ "20"
    , style_ ("width" =: "100%")
    , value_ (messageLog model)
    ]
    []
  , form_
    [class_ "form-inline", onSubmit SendMessage]
    [ input_
      [ type_ "text"
      , class_ "form-control"
      , size_ "30"
      , placeholder_
        (append (Participant.name (participant model)) ": Enter message")
      , value_ (message model)
      , onInput (ChangeField NewMessage)
      ]
    , button_ [class_ "btn btn-primary"] [text "Send"]
    ]
  ]


-- UPDATE

update
  :: Action ChatRoomM
  -> Model ChatRoomM
  -> Effect (Action ChatRoomM) (Model ChatRoomM)
update msg model = case msg of
  ChangeField NewMessage message_ -> noEff (model { message = message_ })

  SendMessage                     -> (model { message = "" }) <# do
    send (ChatCommand.NewMessage (ChatMessage (message model)))
    return NoOp

  HandleWebSocket (WebSocketMessage msg_) ->
    (model { messageLog = append (messageLog model) (ChatMessage.message msg_) }
      )
      <# do
           putStrLn "Message received"
           return NoOp

  HandleWebSocket _ -> noEff model

  GetChatHistory    -> model <# do
    send
      (ChatCommand.Register
        (ChatRegistration (participant model) (chatRoom model))
      )
    putStrLn "Getting History"
    resOrErr <- getChatHistory (ChatRoom.id (chatRoom model))
    case resOrErr of
      Left err -> return (GetChatHistoryError (ms $ show err))
      Right (ChatMessageLog hist) -> return (GetChatHistorySuccess hist)

  GetChatHistorySuccess messageLog_ ->
    noEff (model { messageLog = messageLog_ })

  GetChatHistoryError err_ -> noEff (model { messageLog = err_ })

  NoOp                     -> noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub (Action ChatRoomM)]
subscriptions =
  [websocketSub (URL "ws://localhost:4567/chat") (Protocols []) HandleWebSocket]

