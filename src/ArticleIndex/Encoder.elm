module ArticleIndex.Encoder exposing (toJson)

import ArticleIndex.Core exposing (Article, ArticleIndex)
import Json.Encode as Encode


articleEncoder : Article -> Encode.Value
articleEncoder article =
    Encode.object
        [ ( "id", Encode.string article.id )
        , ( "title", Encode.string article.title )
        , ( "author", Encode.string article.author )
        , ( "relativePath", Encode.string article.relativePath )
        ]


articleIndexEncoder : ArticleIndex -> Encode.Value
articleIndexEncoder index =
    Encode.object
        [ ( "articles", Encode.dict identity articleEncoder index.articles )
        , ( "articleOrder", Encode.list Encode.string index.articleOrder )
        ]


toJson : ArticleIndex -> String
toJson index =
    Encode.encode 0 (articleIndexEncoder index)
