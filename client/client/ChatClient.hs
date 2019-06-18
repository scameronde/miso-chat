{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  ChatClient
Description :  The main Miso module. From here everything starts.

This module switches between Login and the Chat.
-}
module ChatClient
  ( Model
  , Action(NoOp)
  , ChatClient.view
  , ChatClient.update
  , ChatClient.subscriptions
  , ChatClient.initialModel
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )
import           Data.Bifunctor

-- import Module (Module)
import qualified Chat                          as C
import qualified Login                         as L
import qualified NavBar                        as NB


-- MODELS

data Model
    = LoginModel L.Model
    | ChatModel C.Model
    deriving (Show, Eq)


initialModel :: Model
initialModel = LoginModel L.initialModel


-- ACTIONS

data Action
    = LoginAction L.Action
    | ChatAction C.Action
    | NoOp
    deriving (Show, Eq)


-- VIEWS

view :: Model -> View Action
view model = div_
  []
  [ NB.viewNavBar model
  , NB.viewMain [div_ [class_ "view-area"] [viewMainArea model]]
  ]


viewMainArea :: Model -> View Action

viewMainArea (LoginModel lmodel) = fmap LoginAction (L.view lmodel)

viewMainArea (ChatModel cmodel) = fmap ChatAction (C.view cmodel)


-- UPDATE

update :: Action -> Model -> Effect Action Model

update (LoginAction (L.Login participant)) (LoginModel _) =
  ChatModel (C.initialModel participant) <# return (ChatAction C.Init)

update (LoginAction laction) (LoginModel lmodel) =
  bimap LoginAction LoginModel (L.update laction lmodel)

update (ChatAction caction) (ChatModel cmodel) =
  bimap ChatAction ChatModel (C.update caction cmodel)

update _ model =
  noEff model


-- SUBSCRIPTIONS

subscriptions :: [Sub Action]
subscriptions = fmap (mapSub ChatAction) C.subscriptions

