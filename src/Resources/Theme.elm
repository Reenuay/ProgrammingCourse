module Resources.Theme exposing (Theme, ThemeName(..), getTheme)

import Element exposing (Color, rgb255)


type ThemeName
    = Light
    | Dark


type alias Theme =
    { backgroundColor : Color
    , panelColor : Color
    , panelHighlightColor : Color
    , borderColor : Color
    , textColor : Color
    }


getTheme : ThemeName -> Theme
getTheme theme =
    case theme of
        Light ->
            { backgroundColor = rgb255 240 240 240
            , panelColor = rgb255 220 220 220
            , panelHighlightColor = rgb255 230 230 230
            , borderColor = rgb255 200 200 200
            , textColor = rgb255 50 50 50
            }

        Dark ->
            { backgroundColor = rgb255 32 30 36
            , panelColor = rgb255 52 50 56
            , panelHighlightColor = rgb255 62 60 66
            , borderColor = rgb255 72 70 76
            , textColor = rgb255 192 190 196
            }
