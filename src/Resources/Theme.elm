module Resources.Theme exposing (Theme, ThemeName(..), getTheme, toggleTheme)

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
            { backgroundColor = rgb255 232 230 236
            , panelColor = rgb255 212 210 216
            , panelHighlightColor = rgb255 202 200 206
            , borderColor = rgb255 192 190 196
            , textColor = rgb255 42 40 46
            }

        Dark ->
            { backgroundColor = rgb255 32 30 36
            , panelColor = rgb255 52 50 56
            , panelHighlightColor = rgb255 62 60 66
            , borderColor = rgb255 72 70 76
            , textColor = rgb255 192 190 196
            }


toggleTheme : ThemeName -> ThemeName
toggleTheme theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light
