module ArticleIndex.Core exposing (Article, ArticleId, ArticleIndex, addArticle, empty)

import Dict exposing (Dict)


type alias ArticleId =
    String


type alias Article =
    { id : ArticleId
    , title : String
    , author : String
    , relativePath : String
    }


type alias ArticleIndex =
    { articles : Dict ArticleId Article
    , articleOrder : List ArticleId
    }


empty : ArticleIndex
empty =
    { articles = Dict.empty
    , articleOrder = []
    }


addArticle : Article -> ArticleIndex -> ArticleIndex
addArticle article index =
    { index
        | articles = Dict.insert article.id article index.articles
        , articleOrder = article.id :: index.articleOrder
    }
