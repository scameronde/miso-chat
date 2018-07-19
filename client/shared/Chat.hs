{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Chat
  (
    Model
  , Action(Init)
  , Chat.view
#ifdef __GHCJS__
  , Chat.update
  , Chat.subscriptions
#endif
  , Chat.initialModel
  ) where

import           Miso          hiding (action_, model)

import qualified Businesstypes as BT
import qualified ChatRoom      as CR
import qualified ChatRooms     as CRS



-- MODELS

data Model = Model
    { participant    :: BT.Participant
    , chatRoomModel  :: Maybe CR.Model
    , chatRoomsModel :: CRS.Model
    } deriving (Show, Eq)


initialModel :: BT.Participant -> Model
initialModel part = Model part Nothing (CRS.initialModel)


-- ACTIONS

data Action
    = HandleChatRoomsAction CRS.Action
    | HandleChatRoomAction CR.Action
    | Init
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model =
    div_ [ class_ "row" ]
         [ div_ [ class_ "col-md-6" ]
            [ fmap HandleChatRoomsAction (CRS.view (chatRoomsModel model)) ]
        , div_ [ class_ "col-md-6" ]
            [ case (chatRoomModel model) of
                Just crm ->
                    fmap HandleChatRoomAction (CR.view crm)

                Nothing ->
                    div_ [] []
            ]
        ]


#ifdef __GHCJS__
-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model =
    case msg of
        HandleChatRoomsAction (CRS.Selected chatRoom) ->
          (model {chatRoomModel = Just (CR.initialModel (participant model) chatRoom)}) <# do
            return (HandleChatRoomAction CR.GetChatHistory)

        HandleChatRoomsAction (CRS.Deselected) ->
          noEff (model {chatRoomModel = Nothing})

        HandleChatRoomsAction msg_ ->
          let (Effect rm ra) = CRS.update msg_ (chatRoomsModel model)
              newModel = model {chatRoomsModel = rm}
              newAction = fmap (mapSub HandleChatRoomsAction) ra
          in
            Effect newModel newAction

        HandleChatRoomAction msg_ ->
          case (chatRoomModel model) of
            Nothing ->
              noEff model

            Just model_ ->
              let (Effect rm ra) = CR.update msg_ model_
                  newModel = model {chatRoomModel = Just rm}
                  newAction = fmap (mapSub HandleChatRoomAction) ra
              in
                Effect newModel newAction

        Init ->
          let (Effect rm ra) = CRS.update CRS.GetChatRooms (chatRoomsModel model)
              newModel = model {chatRoomsModel = rm}
              newAction = fmap (mapSub HandleChatRoomsAction) ra
          in
            Effect newModel newAction


-- REST-CLIENT

subscriptions :: [ Sub Action ]
subscriptions = fmap (mapSub HandleChatRoomAction) CR.subscriptions

#endif

-- UTILS

