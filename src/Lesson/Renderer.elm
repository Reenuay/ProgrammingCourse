module Lesson.Renderer exposing (render, renderErrors, renderOutcome)

import Common.Util exposing (allSidesZero)
import Element exposing (Element, el, fill, height, none, paddingEach, paragraph, px, spacing, text, textColumn, width)
import Element.Font as Font
import Lesson.Core exposing (Block(..), Lesson, LessonCompilationOutcome, StyledText)
import Mark exposing (Outcome(..))
import Mark.Error as Error exposing (Error)
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


render : Lesson -> List (Element msg)
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
    List.concat
        [ title
        , List.map block lesson.body
        , footer
        ]


renderErrors : List Error -> List (Element msg)
renderErrors errors =
    List.map
        (Error.toHtml Error.Dark >> Element.html)
        errors


renderOutcome : LessonCompilationOutcome -> Element msg
renderOutcome outcome =
    textColumn
        [ width fill
        , height fill
        ]
        (case outcome of
            Mark.Success lesson ->
                render lesson

            Mark.Almost { result, errors } ->
                render result ++ renderErrors errors

            Mark.Failure errors ->
                renderErrors errors
        )