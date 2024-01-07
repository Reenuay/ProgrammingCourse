module Components.ThemeToggle exposing (Config, defaultConfig, toggle)

import Animator exposing (Timeline)
import Element exposing (Color, Element, el, height, none, padding, pointer, px, rgb255, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Resources.Theme exposing (ThemeName(..))


type alias Config =
    { size : Int
    , trackColor : Color
    , thumbColor : Color
    }


defaultConfig : Config
defaultConfig =
    { size = 48
    , trackColor = rgb255 0 0 0
    , thumbColor = rgb255 255 255 255
    }


toggle : Config -> Timeline ThemeName -> msg -> Element msg
toggle config timeline switchMsg =
    let
        halfSize =
            config.size // 2

        paddingValue =
            3

        thumbSize =
            halfSize - paddingValue * 2

        maxOffset =
            config.size - thumbSize - paddingValue * 2

        offset =
            (Animator.linear timeline <|
                \themeName ->
                    if themeName == Dark then
                        Animator.at 0

                    else
                        Animator.at 1
            )
                * toFloat maxOffset
                |> round
    in
    row
        [ Background.color config.trackColor
        , width (px config.size)
        , height (px halfSize)
        , Border.rounded halfSize
        , padding paddingValue
        , onClick switchMsg
        , pointer
        ]
        [ el [ width (px offset) ] none
        , el
            [ width (px thumbSize)
            , height (px thumbSize)
            , Border.rounded (thumbSize // 2)
            , Background.color config.thumbColor
            ]
            none
        ]
