module Lesson.Index.Decoder exposing (lessonIndexDecoder)

import Json.Decode as Decode exposing (Decoder)
import Lesson.Index.Core exposing (LessonEntry, LessonIndex)


lessonEntryDecoder : Decoder LessonEntry
lessonEntryDecoder =
    Decode.map4 LessonEntry
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "path" Decode.string)


lessonIndexDecoder : Decoder LessonIndex
lessonIndexDecoder =
    Decode.map2 LessonIndex
        (Decode.field "lessonEntries" (Decode.dict lessonEntryDecoder))
        (Decode.field "lessonOrder" (Decode.list Decode.string))
