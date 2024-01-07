module Resources.Style exposing (scrollbarThumbColor, textSelectionColor)

import Element exposing (Attribute, Color, htmlAttribute, toRgb)
import Html.Attributes exposing (attribute)


colorToRgba : Color -> String
colorToRgba color =
    let
        { red, green, blue, alpha } =
            toRgb color

        toByte : Float -> Int
        toByte channel =
            channel * 255 |> round
    in
    "rgba("
        ++ String.fromInt (toByte red)
        ++ ","
        ++ String.fromInt (toByte green)
        ++ ","
        ++ String.fromInt (toByte blue)
        ++ ","
        ++ String.fromFloat alpha
        ++ ")"


scrollbarThumbColor : Color -> Attribute msg
scrollbarThumbColor color =
    attribute "style" ("--scrollbar-color: " ++ colorToRgba color ++ ";")
        |> htmlAttribute


textSelectionColor : Color -> Attribute msg
textSelectionColor color =
    attribute "style" ("--text-selection-color: " ++ colorToRgba color ++ ";")
        |> htmlAttribute
