module Lesson.Renderer exposing (render)

import Common.Util exposing (allSidesZero)
import Element exposing (Element, el, fill, height, none, paddingEach, paragraph, px, spacing, text, textColumn, width)
import Element.Font as Font
import Lesson.Core exposing (Block(..), Lesson, StyledText)
import Mark exposing (Outcome(..))
import Resources.FontSize exposing (headingFontSize)


styledText : StyledText -> Element msg
styledText { styles, content } =
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
    el styleList (text content)


block : Block -> Element msg
block block_ =
    case block_ of
        Paragraph text ->
            paragraph [ spacing 10 ] (List.map styledText text)


render : Lesson -> Element msg
render lesson =
    let
        title =
            [ el
                [ Font.size headingFontSize
                , paddingEach { allSidesZero | bottom = 30 }
                , Font.semiBold
                ]
                (text lesson.frontmatter.title)
            ]

        footer =
            [ el [ height (px 60) ] none ]
    in
    textColumn
        [ width fill
        , height fill
        ]
        (List.concat
            [ title
            , List.map block lesson.body
            , footer
            ]
        )
