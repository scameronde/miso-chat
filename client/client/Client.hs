{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  )
where

import           Miso

import qualified ChatClient

main :: IO ()
main = do
  startApp App
    { model         = ChatClient.initialModel
    , view          = ChatClient.view
    , update        = ChatClient.update
    , initialAction = ChatClient.NoOp
    , events        = defaultEvents
    , subs          = ChatClient.subscriptions
    , mountPoint    = Nothing
    }

