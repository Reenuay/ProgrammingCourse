module Resources.Theme exposing (ColorScheme, Theme(..), getCurrentColorScheme, toggle)

import Animator exposing (Timeline)
import Common.Color
import Element exposing (Color, rgb255)


type Theme
    = Light
    | Dark


type alias ColorScheme =
    { backgroundColor : Color
    , panelColor : Color
    , panelHighlightColor : Color
    , borderColor : Color
    , textColor : Color
    , accentColor : Color
    }


toggle : Theme -> Theme
toggle theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light


getColorScheme : Theme -> ColorScheme
getColorScheme theme =
    case theme of
        Light ->
            { backgroundColor = rgb255 232 230 236
            , panelColor = rgb255 212 210 216
            , panelHighlightColor = rgb255 202 200 206
            , borderColor = rgb255 192 190 196
            , textColor = rgb255 42 40 46
            , accentColor = rgb255 255 0 81
            }

        Dark ->
            { backgroundColor = rgb255 32 30 36
            , panelColor = rgb255 52 50 56
            , panelHighlightColor = rgb255 62 60 66
            , borderColor = rgb255 72 70 76
            , textColor = rgb255 192 190 196
            , accentColor = rgb255 255 0 81
            }


getCurrentColorScheme : Timeline Theme -> ColorScheme
getCurrentColorScheme themeName =
    let
        inTransition =
            Animator.current themeName /= Animator.arrived themeName
    in
    if inTransition then
        let
            darkTheme =
                getColorScheme Dark

            lightTheme =
                getColorScheme Light

            mergeColor mapper builder =
                (Animator.color themeName <|
                    \name ->
                        case name of
                            Light ->
                                mapper lightTheme |> Common.Color.fromElementColor

                            Dark ->
                                mapper darkTheme |> Common.Color.fromElementColor
                )
                    |> Common.Color.toElementColor
                    |> builder
        in
        ColorScheme
            |> mergeColor .backgroundColor
            |> mergeColor .panelColor
            |> mergeColor .panelHighlightColor
            |> mergeColor .borderColor
            |> mergeColor .textColor
            |> mergeColor .accentColor

    else
        Animator.current themeName
            |> getColorScheme
