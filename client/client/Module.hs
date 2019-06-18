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
                                                )

-- | Entry point of a module 
data Module model action = Module
  { _model :: model
  -- ^ initial model
  , _action :: action
  -- ^ initial action that should be run for module initialization
  , _view :: model -> View action
  -- ^ function to draw the view of the module
  , _update :: action -> model -> Effect action model
  -- ^ function to update the model of the module
  , _subs :: [ Sub action ]
  -- ^ list of subscriptions to run during the lifetime of the module
  }
