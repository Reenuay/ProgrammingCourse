module Lesson.Index.Core exposing (Index, LessonEntry, addLessonEntry, empty, getLessonsOrdered)

import Dict exposing (Dict)


type alias LessonEntry =
    { id : String
    , title : String
    , author : String
    , path : String
    }


type alias Index =
    { lessonEntries : Dict String LessonEntry
    , lessonOrder : List String
    }


empty : Index
empty =
    { lessonEntries = Dict.empty
    , lessonOrder = []
    }


addLessonEntry : LessonEntry -> Index -> Index
addLessonEntry lessonEntry lessonIndex =
    { lessonIndex
        | lessonEntries = Dict.insert lessonEntry.id lessonEntry lessonIndex.lessonEntries
        , lessonOrder = lessonEntry.id :: lessonIndex.lessonOrder
    }


getLessonsOrdered : Index -> List LessonEntry
getLessonsOrdered lessonIndex =
    List.filterMap (\id -> Dict.get id lessonIndex.lessonEntries) lessonIndex.lessonOrder
