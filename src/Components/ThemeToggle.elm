module Components.ThemeToggle exposing (Config, defaultConfig, toggle)

import Animator exposing (Timeline)
import Common.Color
import Common.Math exposing (lerp)
import Element exposing (Color, Element, centerX, centerY, el, height, html, none, padding, pointer, px, rgb255, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import Resources.Theme exposing (ThemeName(..))


type alias Config =
    { trackWidth : Int
    , trackColor : Color
    , thumbColor : Color
    }


defaultConfig : Config
defaultConfig =
    { trackWidth = 50
    , trackColor = rgb255 0 0 0
    , thumbColor = rgb255 255 255 255
    }


toggle : Config -> Timeline ThemeName -> msg -> Element msg
toggle config timeline switchMsg =
    let
        animationPosition =
            Animator.linear timeline <|
                \themeName ->
                    case themeName of
                        Dark ->
                            Animator.at 0

                        Light ->
                            Animator.at 1

        paddingValue =
            config.trackWidth // 16

        trackHeight =
            config.trackWidth // 2

        thumbSize =
            trackHeight - paddingValue * 2

        maxThumbOffset =
            config.trackWidth - thumbSize - paddingValue * 2

        thumbOffset =
            round (animationPosition * toFloat maxThumbOffset)

        iconAnimationPosition =
            lerp -1 1 animationPosition

        ( icon, iconRelativeSize ) =
            if iconAnimationPosition > 0 then
                ( Material.Icons.brightness_5, 0.8 )

            else
                ( Material.Icons.dark_mode, 1 )

        iconSize =
            round (abs iconAnimationPosition * toFloat thumbSize * iconRelativeSize)
    in
    row
        [ Background.color config.trackColor
        , width (px config.trackWidth)
        , height (px trackHeight)
        , Border.rounded trackHeight
        , padding paddingValue
        , onClick switchMsg
        , pointer
        ]
        [ el [ width (px thumbOffset) ] none
        , el
            [ width (px thumbSize)
            , height (px thumbSize)
            , Border.rounded thumbSize
            , Background.color config.thumbColor
            ]
            (el
                [ width (px iconSize)
                , height (px iconSize)
                , centerX
                , centerY
                ]
                (icon iconSize (Color (Common.Color.fromElementColor config.trackColor)) |> html)
            )
        ]
