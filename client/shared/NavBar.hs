{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module NavBar
  ( NavBar.viewNavBar
  , NavBar.viewMain
  )
where

import           Miso                    hiding ( action_
                                                , model
                                                )


-- VIEWS

viewNavBar :: model -> View action
viewNavBar _ = nav_
  [class_ "navbar navbar-inverse navbar-fixed-top"]
  [ div_
      [class_ "container"]
      [ div_
        [class_ "navbar-header"]
        [ button_
          [ textProp "aria-controls" "navbar"
          , textProp "aria-expanded" "false"
          , class_ "navbar-toggle collapsed"
          , textProp "data-target" "#navbar"
          , textProp "data-toggle" "collapse"
          , type_ "button"
          ]
          [ span_ [class_ "sr-only"]  [text "Toggle navigation"]
          , span_ [class_ "icon-bar"] []
          , span_ [class_ "icon-bar"] []
          , span_ [class_ "icon-bar"] []
          ]
        , a_ [class_ "navbar-brand", href_ ""] [text "Miso Chat"]
        ]
      , div_ [class_ "collapse navbar-collapse", id_ "navbar"]
             [ul_ [class_ "nav navbar-nav"] []]
      , text "     "
      ]
  ]


viewMain :: [View action] -> View action
viewMain elements = div_ [class_ "container"] elements

