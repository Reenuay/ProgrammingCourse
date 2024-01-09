module Article.AST exposing (Article, Block(..), Metadata, StyledText, Styles)


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
