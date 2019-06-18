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

import qualified ChatClient

main :: IO ()
main = 
  startApp App
    { model         = _model ChatClient.desc
    , view          = _view ChatClient.desc
    , update        = _update ChatClient.desc
    , initialAction = _action ChatClient.desc
    , events        = defaultEvents
    , subs          = _subs ChatClient.desc
    , mountPoint    = Nothing
    }
