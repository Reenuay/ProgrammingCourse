module Lesson.Index.Decoder exposing (lessonIndexDecoder)

import Json.Decode as Decode exposing (Decoder)
import Lesson.Index.Core exposing (Index, Lesson)


lessonDecoder : Decoder Lesson
lessonDecoder =
    Decode.map4 Lesson
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "path" Decode.string)


lessonIndexDecoder : Decoder Index
lessonIndexDecoder =
    Decode.map2 Index
        (Decode.field "lessons" (Decode.dict lessonDecoder))
        (Decode.field "lessonOrder" (Decode.list Decode.string))
