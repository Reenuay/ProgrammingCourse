module Resources.Font exposing (baseFont, bodyFontSize, headingFontSize, subheadingFontSize)

import Element
import Element.Font as Font exposing (Font)


scaled : Int -> Int
scaled i =
    Element.modular 20 1.5 i
        |> round


bodyFontSize : Int
bodyFontSize =
    scaled 1


subheadingFontSize : Int
subheadingFontSize =
    scaled 2


headingFontSize : Int
headingFontSize =
    scaled 3


baseFont : List Font
baseFont =
    [ Font.typeface "Segoe UI", Font.sansSerif ]
