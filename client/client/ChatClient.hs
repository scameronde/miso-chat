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
Module      :  ChatClient
Description :  The main Miso module. From here everything starts.

This module switches between Login and the Chat.
-}
module ChatClient
  ( ChatClient (..)
  , Config (ChatClientConfig)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , view
                                                , update
                                                )
import           Data.Bifunctor

import           Module                         ( Module(..) )
import Chat
import Login
import qualified NavBar                        as NB


-- DESCRIPTION

data ChatClient = ChatClient

instance Module ChatClient where

  data Action ChatClient
    = LoginAction (Action Login)
    | ChatAction (Action Chat)
    | Init
    | NoOp
    deriving (Show, Eq)

  data Model ChatClient
    = LoginModel (Model Login)
    | ChatModel (Model Chat)
    deriving (Show, Eq)

  data Config ChatClient = ChatClientConfig

  initialModelM = initialModel

  initialActionM = Init

  viewM = view

  updateM = update

  subscriptionsM = subscriptions


-- MODEL

initialModel :: Config ChatClient -> Model ChatClient
initialModel _ = LoginModel (initialModelM LoginConfig)


-- VIEWS

view :: Model ChatClient -> View (Action ChatClient)
view amodel = div_
    []
    [ NB.viewNavBar amodel
    , NB.viewMain [div_ [class_ "view-area"] [viewMainArea amodel]]
    ]

viewMainArea :: Model ChatClient -> View (Action ChatClient)

viewMainArea (LoginModel lmodel) = fmap LoginAction (viewM lmodel)

viewMainArea (ChatModel  cmodel) = fmap ChatAction (viewM cmodel)


-- UPDATE

update :: Action ChatClient -> Model ChatClient -> Effect (Action ChatClient) (Model ChatClient)

update (LoginAction (Login.LoginParticipant participant)) (LoginModel _) =
  bimap ChatAction ChatModel (updateM initialActionM (initialModelM (ChatConfig participant)))

update (LoginAction laction) (LoginModel lmodel) =
  bimap LoginAction LoginModel (updateM laction lmodel)

update (ChatAction caction) (ChatModel cmodel) =
  bimap ChatAction ChatModel (updateM caction cmodel)

update _ amodel = noEff amodel


-- SUBSCRIPTIONS

subscriptions :: [Sub (Action ChatClient)]
subscriptions = fmap (mapSub ChatAction) subscriptionsM ++ fmap (mapSub LoginAction) subscriptionsM
