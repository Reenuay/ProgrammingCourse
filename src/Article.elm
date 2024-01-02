module Article exposing (..)

import Element exposing (Element, column, el, paragraph, spacing, text)
import Element.Font as Font
import Html.Attributes exposing (style)
import Mark exposing (Block, Document)
import Mark.Error as Error exposing (Error)
import Resources.Font exposing (headingFontSize)


texts : Block (List (Element msg))
texts =
    Mark.text
        (\styles string ->
            let
                styleList =
                    [ ( styles.bold, Font.bold )
                    , ( styles.italic, Font.italic )
                    , ( styles.strike, Font.strike )
                    ]
                        |> List.filterMap
                            (\( condition, style ) ->
                                if condition then
                                    Just style

                                else
                                    Nothing
                            )
            in
            el styleList (text string)
        )


title : Block (Element msg)
title =
    Mark.block "Title"
        (\children -> column [ Font.size headingFontSize, spacing 20 ] children)
        texts


document : Document (List (Element msg))
document =
    Mark.document
        identity
        (Mark.manyOf
            [ title
            , Mark.map (paragraph [ spacing 10 ]) texts
            ]
        )


viewErrors : List Error -> List (Element msg)
viewErrors errors =
    List.map
        (Error.toHtml Error.Dark >> Element.html)
        errors
