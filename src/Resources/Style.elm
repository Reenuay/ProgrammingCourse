module Resources.Style exposing (Style, scrollbarThumbColor, style, textSelectionColor, unselectable)

import Element exposing (Attribute, Color, htmlAttribute, toRgb)
import Html.Attributes exposing (attribute)


type Style
    = ScrollbarThumbColor Color
    | TextSelectionColor Color
    | Unselectable


scrollbarThumbColor : Color -> Style
scrollbarThumbColor =
    ScrollbarThumbColor


textSelectionColor : Color -> Style
textSelectionColor =
    TextSelectionColor


unselectable : Style
unselectable =
    Unselectable


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
        ++ ":rgba("
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

                            Unselectable ->
                                "-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;"
                    )
                |> String.join ""
    in
    attribute "style" css
        |> htmlAttribute
