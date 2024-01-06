module Common.Color exposing (fromElementColor, toElementColor)

import Color
import Element


fromElementColor : Element.Color -> Color.Color
fromElementColor color =
    Element.toRgb color
        |> Color.fromRgba


toElementColor : Color.Color -> Element.Color
toElementColor color =
    Color.toRgba color
        |> Element.fromRgb
