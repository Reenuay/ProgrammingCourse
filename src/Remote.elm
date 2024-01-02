module Remote exposing (view)

import Element exposing (Element, centerX, centerY, el, text)
import Element.Font as Font
import RemoteData exposing (RemoteData(..), WebData)
import Resources.Font exposing (subheadingFontSize)


view : Element msg -> (a -> List (Element msg)) -> WebData a -> List (Element msg)
view loaderView successView remoteData =
    case remoteData of
        NotAsked ->
            [ el [ centerX, centerY, Font.size subheadingFontSize ] (text "No article is open") ]

        Loading ->
            [ el [ centerX, centerY ] loaderView
            ]

        Failure _ ->
            [ el [ centerX, centerY ] (text "Error!") ]

        Success data ->
            successView data
