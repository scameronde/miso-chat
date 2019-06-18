{- |
Module      :  Main
Description :  The interface between Miso and IO ()
-}
module Main
  ( main
  )
where

import           Miso

import qualified ChatClient

main :: IO ()
main = 
  startApp App
    { model         = ChatClient.initialModel
    , view          = ChatClient.view
    , update        = ChatClient.update
    , initialAction = ChatClient.NoOp
    , events        = defaultEvents
    , subs          = ChatClient.subscriptions
    , mountPoint    = Nothing
    }
