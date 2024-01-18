module Lesson.Parser exposing (lesson)

import Lesson.AST exposing (Block(..), Lesson, Metadata, StyledText)
import Mark


metadata : Mark.Block Metadata
metadata =
    Mark.record "Lesson"
        (\title author ->
            { title = title
            , author = author
            }
        )
        |> Mark.field "title" Mark.string
        |> Mark.field "author" Mark.string
        |> Mark.toBlock


styledText : Mark.Block (List StyledText)
styledText =
    Mark.text
        (\styles content ->
            { styles = styles, content = content }
        )


lesson : Mark.Document Lesson
lesson =
    Mark.documentWith
        (\meta body ->
            { metadata = meta
            , body = body
            }
        )
        { metadata = metadata
        , body =
            Mark.manyOf
                [ Mark.map Paragraph styledText
                ]
        }
