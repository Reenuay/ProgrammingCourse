module Loader exposing (..)

import Animator exposing (Timeline)
import Element exposing (Color, Element, alignBottom, el, fill, height, none, padding, px, rgb255, row, spacing, width)
import Element.Background as Background
import Element.Border as Border


type alias Config =
    { size : Int
    , color : Color
    }


defaultConfig : Config
defaultConfig =
    { size = 80
    , color = rgb255 0 0 0
    }


view : Config -> Timeline a -> Element msg
view config timeline =
    let
        paddingValue =
            5

        barsCount =
            3

        maxBarHeight =
            config.size - paddingValue * 2

        animateHeight i =
            Animator.move timeline
                (\_ ->
                    Animator.wave 0.5 1
                        |> Animator.shift (toFloat i / barsCount)
                        |> Animator.loop Animator.slowly
                )
                * toFloat maxBarHeight
                |> round

        bar i =
            el
                [ width fill
                , height (px (animateHeight i))
                , Background.color config.color
                , Border.rounded 5
                , alignBottom
                ]
                none
    in
    row
        [ width (px config.size)
        , height (px config.size)
        , padding paddingValue
        , spacing 5
        ]
        (List.map bar (List.range 0 (barsCount - 1)))
