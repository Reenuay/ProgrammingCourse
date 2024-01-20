port module Lesson.Index.Builder exposing (main)

import Json.Decode exposing (Error(..))
import Lesson.Core exposing (Lesson)
import Lesson.Index.Core as LessonIndex exposing (Index)
import Lesson.Index.Encoder
import Lesson.Parser exposing (lesson)
import Mark exposing (Outcome(..))
import Mark.Error
import Platform exposing (worker)


type alias Content =
    { path : String, content : String }


port receiveInput : (Content -> msg) -> Sub msg


port receiveEndOfInput : (() -> msg) -> Sub msg


port sendResult : String -> Cmd msg


port sendError : String -> Cmd msg


type alias Model =
    { inputHasEnded : Bool
    , lessonIndex : Index
    }


type Msg
    = Input Content
    | EndOfInput


init : () -> ( Model, Cmd msg )
init _ =
    ( { inputHasEnded = False
      , lessonIndex = LessonIndex.empty
      }
    , Cmd.none
    )


parseLesson : String -> Result (List String) Lesson
parseLesson content =
    case Mark.compile lesson content of
        Mark.Success lesson ->
            Ok lesson

        Mark.Almost { result } ->
            Ok result

        Mark.Failure errors ->
            Err <| List.map Mark.Error.toString errors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { path, content } ->
            case parseLesson content of
                Ok { frontmatter } ->
                    let
                        { title, author } =
                            frontmatter
                    in
                    ( { model
                        | lessonIndex =
                            LessonIndex.addLessonEntry
                                { id = title
                                , title = title
                                , author = author
                                , path = path
                                }
                                model.lessonIndex
                      }
                    , Cmd.none
                    )

                Err errors ->
                    ( model, sendError <| String.join "\n" errors )

        EndOfInput ->
            ( { model | inputHasEnded = True }, sendResult <| Lesson.Index.Encoder.toJson model.lessonIndex )


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
