{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  ChatRooms
Description :  Loads and displays the list of available chat rooms. Allows to add and delete chat rooms.

This module polls the list of available chat rooms from the server periodically.
-}
module ChatRooms
  ( ChatRooms(..)
  , Action(Selected, Deselected)
  , Config(ChatRoomsConfig)
  )
where

import           Control.Concurrent
import qualified Data.Foldable                 as F
import qualified Data.List                     as L
import           Miso                    hiding ( action_
                                                , model
                                                , title_
                                                , view
                                                , model
                                                , update
                                                )
import           Miso.String
import           Module                         ( Module(..) )

import           Businesstypes.Id               ( Id(Id) )
import           Businesstypes.ChatRoom         ( ChatRoom(ChatRoom) )
import qualified Businesstypes.ChatRoom        as ChatRoom

import           RestClient


-- MODULE DESCRIPTION

data ChatRooms = ChatRooms

instance Module ChatRooms where

  data Model ChatRooms = Model
    { chatRooms          :: RemoteRefreshingData MisoString [ChatRoom]
    , selectedChatRoomId :: Maybe Id
    , newChatRoomTitle   :: MisoString
    , errorMsg           :: MisoString
    } deriving (Show, Eq)

  data Action ChatRooms
    = Selected ChatRoom
    | Deselected
    | ChangeField Field MisoString
    | SelectChatRoom Id
    | DeleteChatRoom Id
    | DeleteChatRoomError MisoString
    | DeleteChatRoomSuccess
    | PostChatRoom
    | PostChatRoomError MisoString
    | PostChatRoomSuccess
    | GetChatRooms
    | GetChatRoomsError MisoString
    | GetChatRoomsSuccess [ChatRoom]
    deriving (Show, Eq)

  data Config ChatRooms = ChatRoomsConfig

  initialModelM = initialModel

  initialActionM = GetChatRooms

  viewM = view

  updateM = update

  subscriptionsM = []


-- MODELS

data RemoteRefreshingData e a
  = NotAsked
  | Loading
  | Updating a
  | Success a
  | Failure e
  deriving (Show, Eq)

initialModel :: Config ChatRooms -> Model ChatRooms
initialModel _ = Model
  { chatRooms          = NotAsked
  , selectedChatRoomId = Nothing
  , newChatRoomTitle   = ""
  , errorMsg           = ""
  }


-- ACTIONS

data Field = Title
           deriving (Show, Eq)


-- VIEWS

view :: Model ChatRooms -> View (Action ChatRooms)
view model = div_
  []
  [ h2_ [] [text "Chat Room Selection"]
  , viewChatRooms model
  , viewNewChatRoom model
  ]


viewChatRooms :: Model ChatRooms -> View (Action ChatRooms)
viewChatRooms model =
  let (txt :: String, list, selection) = fetchState model
  in  div_
        []
        [ div_ [class_ "info"] [text (pack txt)]
        , viewChatRoomList list selection
        ]


fetchState :: Model ChatRooms -> (String, [ChatRoom], Maybe Id)
fetchState model = case chatRooms model of
  NotAsked   -> ("not asked", [], Nothing)

  Loading    -> ("loading ...", [], Nothing)

  Updating a -> ("updating ...", a, selectedChatRoomId model)

  Success  a -> ("OK", a, selectedChatRoomId model)

  Failure  e -> ("Error: " ++ unpack e, [], Nothing)


viewChatRoomList :: [ChatRoom] -> Maybe Id -> View (Action ChatRooms)
viewChatRoomList chatRooms_ selection_ = table_
  [class_ "table table-striped table-hover"]
  [ thead_
    []
    [tr_ [] [th_ [] [text "Available Chat Rooms"], th_ [] [text "Actions"]]]
  , tbody_
    []
    (   (\chatRoom_ -> tr_
          [class_ (rowClass chatRoom_ selection_)]
          [ td_ [onClick (SelectChatRoom (ChatRoom.id chatRoom_))]
                [text (ChatRoom.title chatRoom_)]
          , td_
            []
            [ button_
                [ class_ "btn btn-danger btn-xs"
                , onClick (DeleteChatRoom (ChatRoom.id chatRoom_))
                ]
                [text "X"]
            ]
          ]
        )
    <$> chatRooms_
    )
  ]


rowClass :: ChatRoom -> Maybe Id -> MisoString
rowClass chatRoom_ selection_ =
  if selection_ == Just (ChatRoom.id chatRoom_) then pack "info" else pack ""


viewNewChatRoom :: Model ChatRooms -> View (Action ChatRooms)
viewNewChatRoom model = form_
  [onSubmit PostChatRoom]
  [ div_
    [class_ "form-group"]
    [ label_ [for_ "titleInput"] [text "New Chat Room"]
    , input_
      [ id_ "titleInput"
      , type_ "text"
      , value_ (newChatRoomTitle model)
      , class_ "form-control"
      , onInput (ChangeField Title)
      ]
    ]
  , button_
    [class_ "btn btn-primary", disabled_ (newChatRoomTitle model == "")]
    [text "Create"]
  ]


-- UPDATE

update
  :: Action ChatRooms
  -> Model ChatRooms
  -> Effect (Action ChatRooms) (Model ChatRooms)
update action model = case action of
  -- select or deselect a chat room
  SelectChatRoom rid_      -> selectOrDeselectChatRoom rid_ model

  -- enter the title for a new chat room
  ChangeField Title title_ -> noEff (model { newChatRoomTitle = title_ })

  -- add a new chat room
  PostChatRoom             -> if newChatRoomTitle model == ""
    then noEff model
    else model <# do
      resOrErr <- postRoom
        (ChatRoom {ChatRoom.id = Id "", ChatRoom.title = newChatRoomTitle model}
        )
      case resOrErr of
        Left  err -> return (PostChatRoomError (ms $ show err))
        Right _   -> return PostChatRoomSuccess

  PostChatRoomError err -> noEff (model { errorMsg = err })

  PostChatRoomSuccess   -> noEff
    (model { errorMsg           = ""
           , selectedChatRoomId = Nothing
           , newChatRoomTitle   = ""
           }
    )

  -- get available chat rooms
  GetChatRooms ->
    let newChatRooms = case chatRooms model of
          Success  a -> Updating a
          Updating a -> Updating a
          _          -> Loading
    in  batchEff
          (model { chatRooms = newChatRooms })
          [ do
            putStrLn "Getting Chat Rooms"
            resOrErr <- getRooms
            case resOrErr of
              Left  err -> return (GetChatRoomsError (ms $ show err))
              Right crs -> return (GetChatRoomsSuccess crs)
          , do
            threadDelay 1000000
            return GetChatRooms
          ]

  GetChatRoomsError err ->
    (model { errorMsg           = err
           , chatRooms          = Failure err
           , selectedChatRoomId = Nothing
           }
      )
      <# return Deselected

  GetChatRoomsSuccess crs  -> updateChatRoomList crs model

  -- delete chat room
  DeleteChatRoom      rid_ -> model <# do
    resOrErr <- deleteRoom rid_
    case resOrErr of
      Left  err -> return (DeleteChatRoomError (ms $ show err))
      Right _   -> return DeleteChatRoomSuccess

  DeleteChatRoomError err_ -> (model { errorMsg = err_ }) <# do
    putStrLn "Error deleting ChatRoom"
    return Deselected

  DeleteChatRoomSuccess -> noEff model

  -- for external communication
  Selected _            -> noEff model

  Deselected            -> noEff model


findChatRoom :: Id -> [ChatRoom] -> Maybe ChatRoom
findChatRoom rid_ = F.find (\cr -> ChatRoom.id cr == rid_)


deselectChatRoom
  :: Model ChatRooms -> Effect (Action ChatRooms) (Model ChatRooms)
deselectChatRoom model =
  (model { selectedChatRoomId = Nothing }) <# return Deselected


selectChatRoom
  :: ChatRoom -> Model ChatRooms -> Effect (Action ChatRooms) (Model ChatRooms)
selectChatRoom cr model =
  (model { selectedChatRoomId = Just (ChatRoom.id cr) }) <# return (Selected cr)


selectOrDeselectChatRoom
  :: Id -> Model ChatRooms -> Effect (Action ChatRooms) (Model ChatRooms)
selectOrDeselectChatRoom rid_ model_ =
  if selectedChatRoomId model_ == Just rid_
    then deselectChatRoom model_
    else case chatRooms model_ of
      Updating crs_ -> selectFromAvailableChatRoom rid_ crs_ model_

      Success  crs_ -> selectFromAvailableChatRoom rid_ crs_ model_

      _             -> deselectChatRoom model_


selectFromAvailableChatRoom
  :: Id
  -> [ChatRoom]
  -> Model ChatRooms
  -> Effect (Action ChatRooms) (Model ChatRooms)
selectFromAvailableChatRoom rid_ chatRooms_ model_ =
  case findChatRoom rid_ chatRooms_ of
    Nothing        -> deselectChatRoom model_

    Just chatRoom_ -> selectChatRoom chatRoom_ model_


updateChatRoomList
  :: [ChatRoom]
  -> Model ChatRooms
  -> Effect (Action ChatRooms) (Model ChatRooms)
updateChatRoomList crs model =
  let newChatRooms = Success (L.sortOn ChatRoom.title crs)
  in  case selectedChatRoomId model of
        Nothing   -> noEff (model { chatRooms = newChatRooms })

        Just rid_ -> case findChatRoom rid_ crs of
          Just _ -> noEff (model { chatRooms = newChatRooms })

          Nothing ->
            (model { chatRooms = newChatRooms, selectedChatRoomId = Nothing })
              <# return Deselected
