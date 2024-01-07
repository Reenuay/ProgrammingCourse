module Resources.FontSize exposing (baseFont, bodyFontSize, giantFontSize, headingFontSize, smallFontSize, subheadingFontSize)

import Element
import Element.Font as Font exposing (Font)


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i
        |> round


smallFontSize : Int
smallFontSize =
    scaled -1


bodyFontSize : Int
bodyFontSize =
    scaled 1


subheadingFontSize : Int
subheadingFontSize =
    scaled 2


headingFontSize : Int
headingFontSize =
    scaled 4


giantFontSize : Int
giantFontSize =
    scaled 6


baseFont : List Font
baseFont =
    [ Font.typeface "Segoe UI", Font.sansSerif ]
