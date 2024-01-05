module Article exposing (CompiledArticle, document, viewErrors)

import Element exposing (Element, el, paragraph, spacing, text)
import Element.Font as Font
import Html.Attributes exposing (style)
import Mark exposing (Block, Document)
import Mark.Error as Error exposing (Error)


type alias Metadata =
    { title : String }


type alias CompiledArticle msg =
    { metadata : Metadata, body : List (Element msg) }


metadata : Block Metadata
metadata =
    Mark.record "Article"
        (\title ->
            { title = title
            }
        )
        |> Mark.field "title" Mark.string
        |> Mark.toBlock


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


document : Document (CompiledArticle msg)
document =
    Mark.documentWith
        (\meta body ->
            { metadata = meta
            , body = body
            }
        )
        { metadata = metadata
        , body =
            Mark.manyOf
                [ Mark.map (paragraph [ spacing 10 ]) texts
                ]
        }


viewErrors : List Error -> List (Element msg)
viewErrors errors =
    List.map
        (Error.toHtml Error.Dark >> Element.html)
        errors
