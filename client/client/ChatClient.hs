{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      :  ChatClient
Description :  The main Miso module. From here everything starts.

This module switches between Login and Chat.
-}
module ChatClient
  ( ChatClient(..)
  , Config(ChatClientConfig)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , view
                                                , update
                                                )
import           Data.Bifunctor

import           Module                         ( Module(..) )
import           Chat
import           Login
import qualified NavBar                        as NB


-- MODULE DEFINITION

data ChatClient = ChatClient

instance Module ChatClient where

  data Action ChatClient
    = LoginAction (Action Login)
    | ChatAction (Action Chat)
    | Init
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
view model = div_
  []
  [ NB.viewNavBar model
  , NB.viewMain [div_ [class_ "view-area"] [viewMainArea model]]
  ]

viewMainArea :: Model ChatClient -> View (Action ChatClient)

viewMainArea (LoginModel model) = fmap LoginAction (viewM model)

viewMainArea (ChatModel  model) = fmap ChatAction (viewM model)


-- UPDATE

update
  :: Action ChatClient
  -> Model ChatClient
  -> Effect (Action ChatClient) (Model ChatClient)

update (LoginAction (Login.LoginParticipant participant)) (LoginModel _) =
  bimap ChatAction
        ChatModel
        (updateM initialActionM (initialModelM (ChatConfig participant)))

update (LoginAction action) (LoginModel model) =
  bimap LoginAction LoginModel (updateM action model)

update (ChatAction action) (ChatModel model) =
  bimap ChatAction ChatModel (updateM action model)

update _ model = noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub (Action ChatClient)]
subscriptions =
  fmap (mapSub ChatAction) subscriptionsM
    ++ fmap (mapSub LoginAction) subscriptionsM
