module Lesson.Index.Core exposing (LessonEntry, LessonIndex, addLessonEntry, empty, getLessonsOrdered)

import Dict exposing (Dict)


type alias LessonEntry =
    { id : String
    , title : String
    , author : String
    , path : String
    }


type alias LessonIndex =
    { lessonEntries : Dict String LessonEntry
    , lessonOrder : List String
    }


empty : LessonIndex
empty =
    { lessonEntries = Dict.empty
    , lessonOrder = []
    }


addLessonEntry : LessonEntry -> LessonIndex -> LessonIndex
addLessonEntry lessonEntry lessonIndex =
    { lessonIndex
        | lessonEntries = Dict.insert lessonEntry.id lessonEntry lessonIndex.lessonEntries
        , lessonOrder = lessonEntry.id :: lessonIndex.lessonOrder
    }


getLessonsOrdered : LessonIndex -> List LessonEntry
getLessonsOrdered lessonIndex =
    List.filterMap (\id -> Dict.get id lessonIndex.lessonEntries) lessonIndex.lessonOrder
