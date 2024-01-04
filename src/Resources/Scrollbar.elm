module Resources.Scrollbar exposing (..)

import Element exposing (Attribute, Color, htmlAttribute, toRgb)
import Html.Attributes exposing (attribute)


color : Color -> Attribute msg
color c =
    let
        { red, green, blue, alpha } =
            toRgb c

        toByte : Float -> Int
        toByte channel =
            channel * 255 |> round

        rgbaColor =
            "rgba("
                ++ String.fromInt (toByte red)
                ++ ","
                ++ String.fromInt (toByte green)
                ++ ","
                ++ String.fromInt (toByte blue)
                ++ ","
                ++ String.fromFloat alpha
                ++ ")"
    in
    attribute "style" ("--scrollbar-color: " ++ rgbaColor ++ ";")
        |> htmlAttribute
