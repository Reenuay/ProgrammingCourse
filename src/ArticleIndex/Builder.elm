port module ArticleIndex.Builder exposing (main)

import Article.AST exposing (Article)
import Article.Parser exposing (article)
import ArticleIndex.Core as Index exposing (ArticleIndex)
import ArticleIndex.Encoder
import Json.Decode exposing (Error(..))
import Mark exposing (Outcome(..))
import Mark.Error
import Platform exposing (worker)


type alias Content =
    { relativePath : String, content : String }


port receiveInput : (Content -> msg) -> Sub msg


port receiveEndOfInput : (() -> msg) -> Sub msg


port sendResult : String -> Cmd msg


port sendError : String -> Cmd msg


type alias Model =
    { inputHasEnded : Bool
    , articleIndex : ArticleIndex
    }


type Msg
    = Input Content
    | EndOfInput


init : () -> ( Model, Cmd msg )
init _ =
    ( { inputHasEnded = False
      , articleIndex = Index.empty
      }
    , Cmd.none
    )


parseArticle : String -> Result (List String) Article
parseArticle content =
    case Mark.compile article content of
        Mark.Success article ->
            Ok article

        Mark.Almost { result } ->
            Ok result

        Mark.Failure errors ->
            Err <| List.map Mark.Error.toString errors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { relativePath, content } ->
            case parseArticle content of
                Ok { metadata } ->
                    let
                        { title, author } =
                            metadata
                    in
                    ( { model
                        | articleIndex =
                            Index.addArticle
                                { id = title
                                , title = title
                                , author = author
                                , relativePath = relativePath
                                }
                                model.articleIndex
                      }
                    , Cmd.none
                    )

                Err errors ->
                    ( model, sendError <| String.join "\n" errors )

        EndOfInput ->
            ( { model | inputHasEnded = True }, sendResult <| ArticleIndex.Encoder.toJson model.articleIndex )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveInput Input
        , receiveEndOfInput (\_ -> EndOfInput)
        ]


main : Program () Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
