{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

{- |
Module      :  Module
Description :  Exports the 'Module' data type that describes modules in the chat app
-}
module Module
  ( Module.Module(..)
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                , set
                                                , update
                                                , view
                                                )

class Module a where
  data Action a
  data Model a
  data Config a

  initialModelM :: Config a -> Model a
  initialActionM :: Action a
  viewM :: Model a -> View (Action a)
  updateM :: Action a -> Model a -> Effect (Action a) (Model a)
  subscriptionsM :: [ Sub (Action a)]
