module ArticleIndex.Decoder exposing (fromJson)

import ArticleIndex.Core exposing (Article, ArticleIndex)
import Json.Decode as Decode exposing (Decoder, Error)


articleDecoder : Decoder Article
articleDecoder =
    Decode.map4 Article
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "reativePath" Decode.string)


articleIndexDecoder : Decoder ArticleIndex
articleIndexDecoder =
    Decode.map2 ArticleIndex
        (Decode.field "articles" (Decode.dict articleDecoder))
        (Decode.field "articleOrder" (Decode.list Decode.string))


fromJson : String -> Result Error ArticleIndex
fromJson json =
    Decode.decodeString articleIndexDecoder json
