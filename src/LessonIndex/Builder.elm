port module LessonIndex.Builder exposing (main)

import Json.Decode exposing (Error(..))
import Lesson.AST exposing (Lesson)
import Lesson.Parser exposing (lesson)
import LessonIndex.Core as LessonIndex exposing (LessonIndex)
import LessonIndex.Encoder
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
    , lessonIndex : LessonIndex
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
                Ok { metadata } ->
                    let
                        { title, author } =
                            metadata
                    in
                    ( { model
                        | lessonIndex =
                            LessonIndex.addLesson
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
            ( { model | inputHasEnded = True }, sendResult <| LessonIndex.Encoder.toJson model.lessonIndex )


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
