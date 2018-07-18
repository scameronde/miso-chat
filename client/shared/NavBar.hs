{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module NavBar
  (
    NavBar.viewNavBar
  , NavBar.viewMain
  ) where

import Data.Proxy
import Miso
import Miso.String
import Data.Text (Text)
import Servant.API
#ifdef __GHCJS__
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient(runClientMOrigin)
#endif




-- VIEWS

viewNavBar :: model -> View action
viewNavBar model =
    nav_ [ class_ "navbar navbar-inverse navbar-fixed-top" ]
        [ div_ [ class_ "container" ]
            [ div_ [ class_ "navbar-header" ]
                [ button_ [ textProp "aria-controls" "navbar"
                          , textProp "aria-expanded" "false"
                          , class_ "navbar-toggle collapsed"
                          , textProp "data-target" "#navbar"
                          , textProp "data-toggle" "collapse"
                          , type_ "button" ]
                    [ span_ [ class_ "sr-only" ]
                        [ text "Toggle navigation" ]
                    , span_ [ class_ "icon-bar" ]
                        []
                    , span_ [ class_ "icon-bar" ]
                        []
                    , span_ [ class_ "icon-bar" ]
                        []
                    ]
                , a_ [ class_ "navbar-brand", href_ "#" ]
                    [ text "Elm Chat" ]
                ]
            , div_ [ class_ "collapse navbar-collapse", id_ "navbar" ]
                [ ul_ [ class_ "nav navbar-nav" ] []
                  {-
                     [ li [ class "active" ]
                         [ a [ href "#" ]
                             [ text "Home" ]
                         ]
                     , li []
                         [ a [ href "#about" ]
                             [ text "About" ]
                         ]
                     , li []
                         [ a [ href "#contact" ]
                             [ text "Contact" ]
                         ]
                     ]
                  -}
                ]
            , text "     "
            ]
        ]


viewMain :: [View action] -> View action
viewMain elements =
    div_ [ class_ "container" ] elements

