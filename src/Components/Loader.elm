module Components.Loader exposing (render)

import Animator exposing (Timeline)
import Element exposing (Color, Element, alignBottom, el, fill, height, none, padding, px, rgb255, row, spacing, width)
import Element.Background as Background
import Element.Border as Border


render : Int -> Color -> Timeline a -> Element msg
render size color timeline =
    let
        paddingValue =
            5

        barsCount =
            3

        maxBarHeight =
            size - paddingValue * 2

        animateHeight i =
            (Animator.move timeline <|
                \_ ->
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
                , Background.color color
                , Border.rounded 5
                , alignBottom
                ]
                none
    in
    row
        [ width (px size)
        , height (px size)
        , padding paddingValue
        , spacing 5
        ]
        (List.map bar (List.range 0 (barsCount - 1)))
