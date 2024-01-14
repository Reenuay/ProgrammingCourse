module ArticleIndex.Decoder exposing (articleIndexDecoder)

import ArticleIndex.Core exposing (Article, ArticleIndex)
import Json.Decode as Decode exposing (Decoder)


articleDecoder : Decoder Article
articleDecoder =
    Decode.map4 Article
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "path" Decode.string)


articleIndexDecoder : Decoder ArticleIndex
articleIndexDecoder =
    Decode.map2 ArticleIndex
        (Decode.field "articles" (Decode.dict articleDecoder))
        (Decode.field "articleOrder" (Decode.list Decode.string))
