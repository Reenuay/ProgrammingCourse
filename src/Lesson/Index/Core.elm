module Lesson.Index.Core exposing (Index, Lesson, addLesson, empty, getLessonsOrdered)

import Dict exposing (Dict)


type alias Lesson =
    { id : String
    , title : String
    , author : String
    , path : String
    }


type alias Index =
    { lessons : Dict String Lesson
    , lessonOrder : List String
    }


empty : Index
empty =
    { lessons = Dict.empty
    , lessonOrder = []
    }


addLesson : Lesson -> Index -> Index
addLesson lesson lessonIndex =
    { lessonIndex
        | lessons = Dict.insert lesson.id lesson lessonIndex.lessons
        , lessonOrder = lesson.id :: lessonIndex.lessonOrder
    }


getLessonsOrdered : Index -> List Lesson
getLessonsOrdered lessonIndex =
    List.filterMap (\id -> Dict.get id lessonIndex.lessons) lessonIndex.lessonOrder
