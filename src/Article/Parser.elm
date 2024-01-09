module Article.Parser exposing (article)

import Article.AST exposing (Article, Block(..), Metadata, StyledText)
import Mark


metadata : Mark.Block Metadata
metadata =
    Mark.record "Article"
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


article : Mark.Document Article
article =
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
