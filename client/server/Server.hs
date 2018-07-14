{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Time.LocalTime
import           Lucid
import           Miso (View, ToServerRoutes)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.WebSockets
import           Options.Applicative
import           Servant
import           Servant.API.WebSocket

import qualified Time
import qualified Counter
import qualified Home
import Businesstypes


data Opts = Opts
  { optPort :: Int
  , optStaticDir :: FilePath
  } deriving (Show, Eq)

optParser :: Parser Opts
optParser =
  Opts
    <$> option auto (short 'p' <> metavar "PORT" <> value 8080)
    <*> strOption (short 'd' <> metavar "DIR" <> value "static")

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper) fullDesc)
  let port = optPort opts
  putStrLn ("Starting server on port " <> show port)
  run port $ logStdoutDev (app (optStaticDir opts))

type API =
       "static" :> Raw
  :<|> "websocket" :> WebSocket
  :<|> Time.GetTimeAPI
  :<|> ServerRoutes

type ServerRoutes = ToServerRoutes Home.ClientRoutes Wrapper Home.Action

newtype Wrapper a = Wrapper a

instance ToHtml a => ToHtml (Wrapper a) where
  toHtml (Wrapper a) =
    doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        link_ [ rel_ "stylesheet" 
              , href_ "static/bootstrap/css/bootstrap.css"]
        link_ [ rel_ "stylesheet" 
              , href_ "static/chat.css"]
        script_ [src_ "static/jquery/jquery.js"] ("" :: Text)
        script_ [src_ "static/bootstrap/js/bootstrap.js"] ("" :: Text)
        script_ [src_ "static/all.js"] ("" :: Text)
      body_ $ do
        toHtml a
  toHtmlRaw = toHtml

app :: FilePath -> Application
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> 
                                    websocketHandler :<|> 
                                    getTimeHandler :<|> 
                                    serverHandlers)

serverHandlers :: Handler (Wrapper (View Home.Action)) :<|> Handler (Wrapper (View Time.Action)) :<|> Handler (Wrapper (View Counter.Action))
serverHandlers = homeHandler :<|> timeHandler :<|> counterHandler
  where homeHandler    = pure (Wrapper (Home.view    (Home.initialModel (Home.getURI @(View Home.Action)))))
        timeHandler    = pure (Wrapper (Time.view    (Time.initialModel)))
        counterHandler = pure (Wrapper (Counter.view (Counter.initialModel)))

staticHandler :: FilePath -> Tagged Handler Application
staticHandler staticDir = serveDirectoryWebApp staticDir


-- BUSINESS SERVICES

websocketHandler :: Connection -> Handler ()
websocketHandler conn = do
  liftIO . forM_ [1::Int ..] $ \i -> do
    sendTextData conn (Text.pack (show (show i))) >> threadDelay 1000000

getTimeHandler :: Handler Time
getTimeHandler = do
  t <- liftIO getZonedTime
  return (Time t)

