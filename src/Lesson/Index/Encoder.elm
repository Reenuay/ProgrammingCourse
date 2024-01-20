module Lesson.Index.Encoder exposing (toJson)

import Json.Encode as Encode exposing (Value)
import Lesson.Index.Core exposing (Index, LessonEntry)


lessonEntryEncoder : LessonEntry -> Value
lessonEntryEncoder lesson =
    Encode.object
        [ ( "id", Encode.string lesson.id )
        , ( "title", Encode.string lesson.title )
        , ( "author", Encode.string lesson.author )
        , ( "path", Encode.string lesson.path )
        ]


lessonIndexEncoder : Index -> Value
lessonIndexEncoder lessonIndex =
    Encode.object
        [ ( "lessonEntries", Encode.dict identity lessonEntryEncoder lessonIndex.lessonEntries )
        , ( "lessonOrder", Encode.list Encode.string lessonIndex.lessonOrder )
        ]


toJson : Index -> String
toJson lessonIndex =
    Encode.encode 0 (lessonIndexEncoder lessonIndex)
