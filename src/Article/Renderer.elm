module Article.Renderer exposing (render, renderErrors)

import Article.AST exposing (Article, Block(..), StyledText)
import Common.Util exposing (allSidesZero)
import Element exposing (Element, el, height, none, paddingEach, paragraph, px, spacing, text)
import Element.Font as Font
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


render : Article -> List (Element msg)
render article =
    let
        title =
            [ el
                [ Font.size headingFontSize
                , paddingEach { allSidesZero | bottom = 30 }
                , Font.semiBold
                ]
                (text article.metadata.title)
            ]

        footer =
            [ el [ height (px 60) ] none ]
    in
    List.concat
        [ title
        , List.map block article.body
        , footer
        ]


renderErrors : List Error -> List (Element msg)
renderErrors errors =
    List.map
        (Error.toHtml Error.Dark >> Element.html)
        errors
