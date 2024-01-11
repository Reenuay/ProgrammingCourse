module Article.AST exposing (Article, ArticleCompilationOutcome, Block(..), Metadata, StyledText, Styles)

import Mark exposing (Outcome, Partial)
import Mark.Error exposing (Error)


type alias Metadata =
    { title : String, author : String }


type alias Styles =
    { bold : Bool, italic : Bool, strike : Bool }


type alias StyledText =
    { styles : Styles, content : String }


type Block
    = Paragraph (List StyledText)


type alias Article =
    { metadata : Metadata, body : List Block }


type alias ArticleCompilationOutcome =
    Outcome (List Error) (Partial Article) Article
