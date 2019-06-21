{- |
Module      :  Main
Description :  The interface between Miso and IO ()
-}
module Main
  ( main
  )
where

import           Miso
import           Module                         ( Module(..) )

import  ChatClient

main :: IO ()
main = 
  startApp App
    { model         = initialModelM ChatClientConfig
    , view          = viewM
    , update        = updateM
    , initialAction = initialActionM
    , events        = defaultEvents
    , subs          = subscriptionsM
    , mountPoint    = Nothing
    }
