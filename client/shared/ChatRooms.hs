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
import           Miso               hiding (action_, model, title_)
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
    | PostChatRoom
    | PostChatRoomError MisoString
    | PostChatRoomSuccess
    | GetChatRooms
    | GetChatRoomsError MisoString
    | GetChatRoomsSuccess [ChatRoom]
    | NoOp
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
viewChatRoomList chatRooms_ selection_ =
    table_ [ class_ "table table-striped table-hover" ]
        [ thead_ []
            [ tr_ []
                [ th_ [] [ text "Available Chat Rooms" ]
                , th_ [] [ text "Actions" ]
                ]
            ]
        , tbody_ []
            (
                (\chatRoom_ ->
                    tr_ [ class_ (rowClass chatRoom_ selection_) ]
                        [ td_ [ onClick (SelectChatRoom (rid chatRoom_)) ]
                            [ text (title chatRoom_) ]
                        , td_ []
                            [ button_
                                [ class_ "btn btn-danger btn-xs"
                                , onClick (DeleteChatRoom (rid chatRoom_))
                                ]
                                [ text "X" ]
                            ]
                        ]
                ) <$> chatRooms_
            )
        ]


rowClass :: ChatRoom -> Maybe Id -> MisoString
rowClass chatRoom_ selection_ =
  if (selection_ == Just (rid chatRoom_)) then
    pack "info"
  else
    pack ""


viewNewChatRoom :: Model -> View Action
viewNewChatRoom model =
  form_ [ onSubmit PostChatRoom ]
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
        SelectChatRoom rid_ ->
            selectOrDeselectChatRoom rid_ model

        -- enter the title for a new chat room
        ChangeField Title title_ ->
            noEff (model { newChatRoomTitle = title_ })

        -- add a new chat room
        PostChatRoom ->
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
        DeleteChatRoom rid_ ->
          model <# do
            resOrErr <- deleteRoom rid_
            case resOrErr of
              Left err -> return (DeleteChatRoomError (ms $ show err))
              Right _  -> return DeleteChatRoomSuccess

        DeleteChatRoomError err_ ->
          (model {errorMsg = err_}) <# do
            putStrLn "Error deleting ChatRoom"
            return Deselected

        DeleteChatRoomSuccess ->
          noEff model

        NoOp ->
            noEff model

        -- for external communication
        Selected _ ->
            noEff model

        Deselected ->
            noEff model


findChatRoom :: Id -> [ChatRoom] -> Maybe ChatRoom
findChatRoom rid_ crs_ =
    F.find (\cr -> (rid cr) == rid_) crs_


deselectChatRoom :: Model -> Effect Action Model
deselectChatRoom model =
  (model { selectedChatRoomId = Nothing }) <# return Deselected


selectChatRoom :: ChatRoom -> Model -> Effect Action Model
selectChatRoom cr model =
  (model { selectedChatRoomId = Just (rid cr) }) <# return (Selected cr)


selectOrDeselectChatRoom :: Id -> Model -> Effect Action Model
selectOrDeselectChatRoom rid_ model_ =
    if (selectedChatRoomId model_ == Just rid_) then
        deselectChatRoom model_
    else
        case (chatRooms model_) of
            Updating crs_ ->
                selectFromAvailableChatRoom rid_ crs_ model_

            Success crs_ ->
                selectFromAvailableChatRoom rid_ crs_ model_

            _ ->
                deselectChatRoom model_


selectFromAvailableChatRoom :: Id -> [ChatRoom] -> Model -> Effect Action Model
selectFromAvailableChatRoom rid_ chatRooms_ model_ =
    case findChatRoom rid_ chatRooms_ of
        Nothing ->
            deselectChatRoom model_

        Just chatRoom_ ->
            selectChatRoom chatRoom_ model_


updateChatRoomList :: [ChatRoom] -> Model -> Effect Action Model
updateChatRoomList crs model =
  let
    newChatRooms = Success (L.sortOn title crs)

  in
    case (selectedChatRoomId model) of
      Nothing ->
        noEff (model {chatRooms = newChatRooms})

      Just rid_ ->
        case (findChatRoom rid_ crs) of
          Just _ ->
            noEff (model {chatRooms = newChatRooms})

          Nothing ->
            (model {chatRooms = newChatRooms, selectedChatRoomId = Nothing}) <# return Deselected


