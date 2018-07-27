{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import qualified Businesstypes                 as BT
import qualified ChatRoom                      as CR
import qualified ChatRooms                     as CRS
import           Util
import           Control.Lens



-- MODELS

data Model = Model
    { _participant    :: BT.Participant
    , _chatRoomModel  :: Maybe CR.Model
    , _chatRoomsModel :: CRS.Model
    } deriving (Show, Eq)

makeLenses ''Model

initialModel :: BT.Participant -> Model
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
         [fmap ChatRoomsAction (CRS.view (model ^. chatRoomsModel))]
  , div_
    [class_ "col-md-6"]
    [ case (model ^. chatRoomModel) of
        Just crm -> fmap ChatRoomAction (CR.view crm)

        Nothing  -> div_ [] []
    ]
  ]


-- UPDATE

update :: Action -> Model -> Effect Action Model
update msg model = case msg of
  ChatRoomsAction (CRS.Selected chatRoom) ->
    (let crm = Just (CR.initialModel (model ^. participant) chatRoom)
     in  (chatRoomModel .~ crm) model
      )
      <# do
           return (ChatRoomAction CR.GetChatHistory)

  ChatRoomsAction (CRS.Deselected) -> noEff (chatRoomModel .~ Nothing $ model)

  ChatRoomsAction msg_ -> mapEff CRS.update
                                 msg_
                                 (model ^. chatRoomsModel)
                                 ChatRoomsAction
                                 (\rm -> model { _chatRoomsModel = rm })

  ChatRoomAction msg_ -> case (model ^. chatRoomModel) of
    Nothing     -> noEff model

    Just model_ -> mapEff CR.update
                          msg_
                          model_
                          ChatRoomAction
                          (\rm -> model { _chatRoomModel = Just rm })

  Init -> mapEff CRS.update
                 CRS.GetChatRooms
                 (model ^. chatRoomsModel)
                 ChatRoomsAction
                 (\rm -> model { _chatRoomsModel = rm })


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = fmap (mapSub ChatRoomAction) CR.subscriptions

