module ArticleIndex.Encoder exposing (toJson)

import ArticleIndex.Core exposing (Article, ArticleIndex)
import Json.Encode as Encode exposing (Value)


articleEncoder : Article -> Value
articleEncoder article =
    Encode.object
        [ ( "id", Encode.string article.id )
        , ( "title", Encode.string article.title )
        , ( "author", Encode.string article.author )
        , ( "path", Encode.string article.path )
        ]


articleIndexEncoder : ArticleIndex -> Value
articleIndexEncoder index =
    Encode.object
        [ ( "articles", Encode.dict identity articleEncoder index.articles )
        , ( "articleOrder", Encode.list Encode.string index.articleOrder )
        ]


toJson : ArticleIndex -> String
toJson index =
    Encode.encode 0 (articleIndexEncoder index)
