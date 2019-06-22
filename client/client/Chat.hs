{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      :  Chat
Description :  Coordinates the list of chat rooms and the selected chat room.

-}
module Chat
  ( Chat(..)
  , Config(ChatConfig)
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
import           ChatRooms


-- MODULE DEFINITION

data Chat = Chat

instance Module Chat where

  data Model Chat = Model
    { participant    :: Participant
    , chatRoomModel  :: Maybe CR.Model
    , chatRoomsModel :: Model ChatRooms
    } deriving (Show, Eq)

  data Action Chat
      = ChatRoomsAction (Action ChatRooms)
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
  , chatRoomsModel = initialModelM ChatRoomsConfig
  }


-- VIEWS

view :: Model Chat -> View (Action Chat)
view model = div_
  [class_ "row"]
  [ div_ [class_ "col-md-6"]
         [fmap ChatRoomsAction (viewM (chatRoomsModel model))]
  , div_
    [class_ "col-md-6"]
    [ case chatRoomModel model of
        Just crm -> fmap ChatRoomAction (CR.view crm)

        Nothing  -> div_ [] []
    ]
  ]


-- UPDATE

update :: Action Chat -> Model Chat -> Effect (Action Chat) (Model Chat)

update (ChatRoomsAction (ChatRooms.Selected chatRoom)) model =
  let crm = CR.initialModel (participant model) chatRoom
  in  model { chatRoomModel = Just crm }
        <# return (ChatRoomAction CR.GetChatHistory)

update (ChatRoomsAction ChatRooms.Deselected) model =
  noEff model { chatRoomModel = Nothing }

update (ChatRoomsAction craction) model = bimap
  ChatRoomsAction
  (\rm -> model { chatRoomsModel = rm })
  (updateM craction (chatRoomsModel model))

update (ChatRoomAction craction) model = case chatRoomModel model of
  Nothing      -> noEff model
  Just crmodel -> bimap ChatRoomAction
                        (\rm -> model { chatRoomModel = Just rm })
                        (CR.update craction crmodel)

update _ model = noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub (Action Chat)]
subscriptions = fmap (mapSub ChatRoomAction) CR.subscriptions
