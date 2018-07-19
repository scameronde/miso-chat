{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ChatRooms
  (
    Model
  , Action(Selected, Deselected, GetChatRooms)
  , Field
  , ChatRooms.view
  , ChatRooms.update
  , ChatRooms.initialModel
  ) where

import           Control.Concurrent
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Miso               hiding (action_, model)
import           Miso.String

import           Businesstypes
import           RestClient

-- MODELS

data RemoteRefreshingData e a
  = NotAsked
  | Loading
  | Updating a
  | Success a
  | Failure e
  deriving (Show, Eq)


data Model = Model
  { chatRooms          :: RemoteRefreshingData MisoString [ChatRoom]
  , selectedChatRoomId :: Maybe Id
  , newChatRoomTitle   :: MisoString
  , errorMsg           :: MisoString
  } deriving (Show, Eq)


initialModel :: Model
initialModel =
  Model { chatRooms = NotAsked
  , selectedChatRoomId = Nothing
  , newChatRoomTitle = ""
  , errorMsg = ""
  }


-- ACTIONS

data Field = Title
           deriving (Show, Eq)

data Action
    = Selected ChatRoom
    | Deselected
    | ChangeField Field MisoString
    | SelectChatRoom Id
    | DeleteChatRoom Id
    | DeleteChatRoomError MisoString
    | DeleteChatRoomSuccess
    | PostChatRoom Model
    | PostChatRoomError MisoString
    | PostChatRoomSuccess
    | GetChatRooms
    | GetChatRoomsError MisoString
    | GetChatRoomsSuccess [ChatRoom]
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model =
  div_ []
       [ h2_ [] [ text "Chat Room Selection" ]
       , viewChatRooms model
       , viewNewChatRoom model
       ]


viewChatRooms :: Model -> View Action
viewChatRooms model =
    let
        ( txt::String, list, selection ) = fetchState model
    in
        div_ []
            [ div_ [ class_ "info" ] [ text (pack txt) ]
            , viewChatRoomList list selection
            ]


fetchState :: Model -> (String, [ChatRoom], Maybe Id)
fetchState model =
    case (chatRooms model) of
        NotAsked ->
            ( "not asked", [], Nothing )

        Loading ->
            ( "loading ...", [], Nothing )

        Updating a ->
            ( "updating ...", a, selectedChatRoomId model )

        Success a ->
            ( "OK", a, selectedChatRoomId model )

        Failure e ->
            ( "Error: " ++ (unpack e), [], Nothing )


viewChatRoomList :: [ChatRoom] -> Maybe Id -> View Action
viewChatRoomList chatRooms selection =
    table_ [ class_ "table table-striped table-hover" ]
        [ thead_ []
            [ tr_ []
                [ th_ [] [ text "Available Chat Rooms" ]
                , th_ [] [ text "Actions" ]
                ]
            ]
        , tbody_ []
            (
                (\chatRoom ->
                    tr_ [ class_ (rowClass chatRoom selection) ]
                        [ td_ [ onClick (SelectChatRoom (rid chatRoom)) ]
                            [ text (title chatRoom) ]
                        , td_ []
                            [ button_
                                [ class_ "btn btn-danger btn-xs"
                                , onClick (DeleteChatRoom (rid chatRoom))
                                ]
                                [ text "X" ]
                            ]
                        ]
                ) <$> chatRooms
            )
        ]


rowClass :: ChatRoom -> Maybe Id -> MisoString
rowClass chatRoom selection =
  if (selection == Just (rid chatRoom)) then
    pack "info"
  else
    pack ""


viewNewChatRoom :: Model -> View Action
viewNewChatRoom model =
  form_ [ onSubmit (PostChatRoom model) ]
        [ div_ [ class_ "form-group" ]
               [ label_ [ for_ "titleInput" ] [ text "New Chat Room" ]
               , input_ [ id_ "titleInput", type_ "text", value_ (newChatRoomTitle model), class_ "form-control", onInput (ChangeField Title) ]
               ]
        , button_
            [ class_ "btn btn-primary"
            , disabled_ ((newChatRoomTitle model) == "")
            ]
            [ text "Create" ]
        ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update action model =
    case action of
        -- select or deselect a chat room
        SelectChatRoom id ->
            selectOrDeselectChatRoom id model

        -- enter the title for a new chat room
        ChangeField Title title ->
            noEff (model { newChatRoomTitle = title })

        -- add a new chat room
        PostChatRoom model ->
          if (newChatRoomTitle model == "") then
            noEff model
          else
            model <# do
              resOrErr <- postRoom (ChatRoom {rid = Id "", title = (newChatRoomTitle model)})
              case resOrErr of
                Left err -> return (PostChatRoomError (ms $ show err))
                Right _  -> return PostChatRoomSuccess

        PostChatRoomError err ->
          noEff (model {errorMsg = err})

        PostChatRoomSuccess ->
          noEff (model { errorMsg = "", selectedChatRoomId = Nothing, newChatRoomTitle = "" })

        -- get available chat rooms
        GetChatRooms ->
          let newChatRooms = case (chatRooms model) of
                               Success a  -> Updating a
                               Updating a -> Updating a
                               _          -> Loading
          in
            batchEff (model {chatRooms = newChatRooms})
                     [ do
                         putStrLn "Getting Chat Rooms"
                         resOrErr <- getRooms
                         case resOrErr of
                           Left err  -> return (GetChatRoomsError (ms $ show err))
                           Right crs -> return (GetChatRoomsSuccess crs)
                     , do
                         threadDelay 1000000
                         return GetChatRooms
                     ]

        GetChatRoomsError err ->
          (model {errorMsg = err, chatRooms = Failure err, selectedChatRoomId = Nothing}) <# return Deselected

        GetChatRoomsSuccess crs ->
            updateChatRoomList crs model

        -- delete chat room
        DeleteChatRoom id ->
          model <# do
            resOrErr <- deleteRoom id
            case resOrErr of
              Left err -> return (DeleteChatRoomError (ms $ show err))
              Right _  -> return DeleteChatRoomSuccess

        DeleteChatRoomError err ->
          (model {errorMsg = err}) <# do
            putStrLn "Error deleting ChatRoom"
            return Deselected

        DeleteChatRoomSuccess ->
          noEff model

        -- for external communication
        Selected _ ->
            noEff model

        Deselected ->
            noEff model


findChatRoom :: Id -> [ChatRoom] -> Maybe ChatRoom
findChatRoom id crs =
    F.find (\cr -> (rid cr) == id) crs


deselectChatRoom :: Model -> Effect Action Model
deselectChatRoom model =
  (model { selectedChatRoomId = Nothing }) <# return Deselected


selectChatRoom :: ChatRoom -> Model -> Effect Action Model
selectChatRoom cr model =
  (model { selectedChatRoomId = Just (rid cr) }) <# return (Selected cr)


selectOrDeselectChatRoom :: Id -> Model -> Effect Action Model
selectOrDeselectChatRoom id model =
    if (selectedChatRoomId model == Just id) then
        deselectChatRoom model
    else
        case (chatRooms model) of
            Updating a ->
                selectFromAvailableChatRoom id a model

            Success a ->
                selectFromAvailableChatRoom id a model

            _ ->
                deselectChatRoom model


selectFromAvailableChatRoom :: Id -> [ChatRoom] -> Model -> Effect Action Model
selectFromAvailableChatRoom id chatRooms model =
    case findChatRoom id chatRooms of
        Nothing ->
            deselectChatRoom model

        Just chatRoom ->
            selectChatRoom chatRoom model


updateChatRoomList :: [ChatRoom] -> Model -> Effect Action Model
updateChatRoomList crs model =
  let
    newChatRooms = Success (L.sortOn title crs)

  in
    case (selectedChatRoomId model) of
      Nothing ->
        noEff (model {chatRooms = newChatRooms})

      Just id ->
        case (findChatRoom id crs) of
          Just cr ->
            noEff (model {chatRooms = newChatRooms})

          Nothing ->
            (model {chatRooms = newChatRooms, selectedChatRoomId = Nothing}) <# return Deselected


