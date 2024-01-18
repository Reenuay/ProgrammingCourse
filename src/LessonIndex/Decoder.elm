module LessonIndex.Decoder exposing (lessonIndexDecoder)

import Json.Decode as Decode exposing (Decoder)
import LessonIndex.Core exposing (Lesson, LessonIndex)


lessonDecoder : Decoder Lesson
lessonDecoder =
    Decode.map4 Lesson
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "path" Decode.string)


lessonIndexDecoder : Decoder LessonIndex
lessonIndexDecoder =
    Decode.map2 LessonIndex
        (Decode.field "lessons" (Decode.dict lessonDecoder))
        (Decode.field "lessonOrder" (Decode.list Decode.string))
