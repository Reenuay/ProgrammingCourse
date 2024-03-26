module Components.ThemeToggle exposing (render)

import Animator exposing (Timeline)
import Common.Color
import Common.Math exposing (lerp)
import Element exposing (Color, Element, centerX, centerY, el, height, html, none, padding, pointer, px, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import Resources.Theme exposing (Theme(..))


render : Int -> Color -> Color -> msg -> Timeline Theme -> Element msg
render trackWidth trackColor thumbColor onToggle theme =
    let
        animationPosition =
            Animator.linear theme <|
                \themeName ->
                    case themeName of
                        Dark ->
                            Animator.at 0

                        Light ->
                            Animator.at 1

        paddingValue =
            trackWidth // 16

        trackHeight =
            trackWidth // 2

        thumbSize =
            trackHeight - paddingValue * 2

        maxThumbOffset =
            trackWidth - thumbSize - paddingValue * 2

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
        [ Background.color trackColor
        , width (px trackWidth)
        , height (px trackHeight)
        , Border.rounded trackHeight
        , padding paddingValue
        , onClick onToggle
        , pointer
        ]
        [ el [ width (px thumbOffset) ] none
        , el
            [ width (px thumbSize)
            , height (px thumbSize)
            , Border.rounded thumbSize
            , Background.color thumbColor
            ]
            (el
                [ width (px iconSize)
                , height (px iconSize)
                , centerX
                , centerY
                ]
                (icon iconSize (Color (Common.Color.fromElementColor trackColor)) |> html)
            )
        ]
