{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  Chat
Description :  Coordinates the list of chat rooms and the selected chat room.

This module has to be initialized by the 'Init' action in addition to the 'initialModel'.
-}
module Chat
  ( Model
  , Action(Init)
  , Chat.view
  , Chat.update
  , Chat.subscriptions
  , Chat.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , set
                                                )
import           Data.Bifunctor

import           Businesstypes.Participant      ( Participant )
import qualified ChatRoom                      as CR
import qualified ChatRooms                     as CRS


-- MODELS

data Model = Model
    { participant    :: Participant
    , chatRoomModel  :: Maybe CR.Model
    , chatRoomsModel :: CRS.Model
    } deriving (Show, Eq)

initialModel :: Participant -> Model
initialModel part = Model part Nothing CRS.initialModel


-- ACTIONS

data Action
    = ChatRoomsAction CRS.Action
    | ChatRoomAction CR.Action
    | Init
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model = div_
  [class_ "row"]
  [ div_ [class_ "col-md-6"]
         [fmap ChatRoomsAction (CRS.view (chatRoomsModel model))]
  , div_
    [class_ "col-md-6"]
    [ case chatRoomModel model of
        Just crm -> fmap ChatRoomAction (CR.view crm)

        Nothing  -> div_ [] []
    ]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model

update (ChatRoomsAction (CRS.Selected chatRoom)) model =
  let crm = CR.initialModel (participant model) chatRoom
  in model { chatRoomModel = Just crm} <# return (ChatRoomAction CR.GetChatHistory)

update (ChatRoomsAction CRS.Deselected) model =
  noEff (model { chatRoomModel = Nothing })

update (ChatRoomsAction craction) model =
  bimap ChatRoomsAction (\rm -> model { chatRoomsModel = rm }) (CRS.update craction (chatRoomsModel model))

update (ChatRoomAction craction) model =
  case chatRoomModel model of
    Nothing -> noEff model
    Just crmodel -> bimap ChatRoomAction (\rm -> model { chatRoomModel = Just rm }) (CR.update craction crmodel)
            
update Init model =
  bimap ChatRoomsAction (\rm -> model { chatRoomsModel = rm }) (CRS.update CRS.GetChatRooms (chatRoomsModel model))


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = fmap (mapSub ChatRoomAction) CR.subscriptions

