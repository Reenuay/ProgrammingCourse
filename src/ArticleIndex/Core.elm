module ArticleIndex.Core exposing (Article, ArticleIndex, addArticle, empty, getArticlesOrdered)

import Dict exposing (Dict)


type alias Article =
    { id : String
    , title : String
    , author : String
    , path : String
    }


type alias ArticleIndex =
    { articles : Dict String Article
    , articleOrder : List String
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


getArticlesOrdered : ArticleIndex -> List Article
getArticlesOrdered index =
    List.filterMap (\id -> Dict.get id index.articles) index.articleOrder
