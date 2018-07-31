{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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

import           Businesstypes.Participant      ( Participant )
import qualified ChatRoom                      as CR
import qualified ChatRooms                     as CRS
import           Util


-- MODELS

data Model = Model
    { participant    :: Participant
    , chatRoomModel  :: Maybe CR.Model
    , chatRoomsModel :: CRS.Model
    } deriving (Show, Eq)

initialModel :: Participant -> Model
initialModel part = Model part Nothing (CRS.initialModel)


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
    [ case (chatRoomModel model) of
        Just crm -> fmap ChatRoomAction (CR.view crm)

        Nothing  -> div_ [] []
    ]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model = case msg of
  ChatRoomsAction (CRS.Selected chatRoom) ->
    (let crm = Just (CR.initialModel (participant model) chatRoom)
     in  model { chatRoomModel = crm }
      )
      <# do
           return (ChatRoomAction CR.GetChatHistory)

  ChatRoomsAction (CRS.Deselected) -> noEff (model { chatRoomModel = Nothing })

  ChatRoomsAction msg_ -> mapEff CRS.update
                                 msg_
                                 (chatRoomsModel model)
                                 ChatRoomsAction
                                 (\rm -> model { chatRoomsModel = rm })

  ChatRoomAction msg_ -> case (chatRoomModel model) of
    Nothing     -> noEff model

    Just model_ -> mapEff CR.update
                          msg_
                          model_
                          ChatRoomAction
                          (\rm -> model { chatRoomModel = Just rm })

  Init -> mapEff CRS.update
                 CRS.GetChatRooms
                 (chatRoomsModel model)
                 ChatRoomsAction
                 (\rm -> model { chatRoomsModel = rm })


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = fmap (mapSub ChatRoomAction) CR.subscriptions

