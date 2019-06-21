{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      :  Chat
Description :  Coordinates the list of chat rooms and the selected chat room.

This module has to be initialized by the 'Init' action in addition to the 'initialModel'.
-}
module Chat
  ( Chat (..)
  , Config (ChatConfig)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , set
                                                , view
                                                , update
                                                )
import           Data.Bifunctor

import           Module                         ( Module(..) )
import           Businesstypes.Participant      ( Participant )
import qualified ChatRoom                      as CR
import qualified ChatRooms                     as CRS


data Chat = Chat

instance Module Chat where

  data Model Chat = Model
    { participant    :: Participant
    , chatRoomModel  :: Maybe CR.Model
    , chatRoomsModel :: CRS.Model
    } deriving (Show, Eq)

  data Action Chat
      = ChatRoomsAction CRS.Action
      | ChatRoomAction CR.Action
      | Init
      deriving (Show, Eq)

  data Config Chat = ChatConfig Participant

  initialModelM = initialModel

  initialActionM = Init

  viewM = view

  updateM = update

  subscriptionsM = subscriptions


-- MODEL

initialModel :: Config Chat -> Model Chat
initialModel (ChatConfig part) = Model 
  { participant    = part
  , chatRoomModel  = Nothing
  , chatRoomsModel = CRS.initialModel
  }


-- VIEWS

view :: Model Chat -> View (Action Chat)
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

update :: Action Chat -> Model Chat -> Effect (Action Chat) (Model Chat)

update (ChatRoomsAction (CRS.Selected chatRoom)) model =
  let crm = CR.initialModel (participant model) chatRoom
  in model { chatRoomModel = Just crm} <# return (ChatRoomAction CR.GetChatHistory)

update (ChatRoomsAction CRS.Deselected) model =
  noEff model { chatRoomModel = Nothing }

update (ChatRoomsAction craction) model =
  bimap ChatRoomsAction (\rm -> model { chatRoomsModel = rm }) (CRS.update craction (chatRoomsModel model))

update (ChatRoomAction craction) model =
  case chatRoomModel model of
    Nothing -> noEff model
    Just crmodel -> bimap ChatRoomAction (\rm -> model { chatRoomModel = Just rm }) (CR.update craction crmodel)

update _ model =
  noEff model
    
    
-- SUBSCRIPTIONS

subscriptions :: [ Sub (Action Chat) ]
subscriptions = fmap (mapSub ChatRoomAction) CR.subscriptions
