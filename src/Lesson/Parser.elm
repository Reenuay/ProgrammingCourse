module Lesson.Parser exposing (lesson)

import Lesson.Core exposing (Block(..), Frontmatter, Lesson, StyledText)
import Mark


metadata : Mark.Block Frontmatter
metadata =
    Mark.record "Frontmatter"
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
            { frontmatter = meta
            , body = body
            }
        )
        { metadata = metadata
        , body =
            Mark.manyOf
                [ Mark.map Paragraph styledText
                ]
        }
