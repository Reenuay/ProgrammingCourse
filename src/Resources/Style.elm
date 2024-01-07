module Resources.Style exposing (Style(..), style)

import Element exposing (Attribute, Color, htmlAttribute, toRgb)
import Html.Attributes exposing (attribute)


type Style
    = ScrollbarThumbColor Color
    | TextSelectionColor Color


colorToStyleEntry : String -> Color -> String
colorToStyleEntry property color =
    let
        { red, green, blue, alpha } =
            toRgb color

        toByte : Float -> Int
        toByte channel =
            channel * 255 |> round
    in
    property
        ++ ": rgba("
        ++ String.fromInt (toByte red)
        ++ ","
        ++ String.fromInt (toByte green)
        ++ ","
        ++ String.fromInt (toByte blue)
        ++ ","
        ++ String.fromFloat alpha
        ++ ");"


style : List Style -> Attribute msg
style styles =
    let
        css =
            styles
                |> List.map
                    (\s ->
                        case s of
                            ScrollbarThumbColor color ->
                                colorToStyleEntry "--scrollbar-color" color

                            TextSelectionColor color ->
                                colorToStyleEntry "--text-selection-color" color
                    )
                |> String.join ""
    in
    attribute "style" css
        |> htmlAttribute
