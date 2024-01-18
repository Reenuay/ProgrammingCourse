module LessonIndex.Core exposing (Lesson, LessonIndex, addLesson, empty, getLessonsOrdered)

import Dict exposing (Dict)


type alias Lesson =
    { id : String
    , title : String
    , author : String
    , path : String
    }


type alias LessonIndex =
    { lessons : Dict String Lesson
    , lessonOrder : List String
    }


empty : LessonIndex
empty =
    { lessons = Dict.empty
    , lessonOrder = []
    }


addLesson : Lesson -> LessonIndex -> LessonIndex
addLesson lesson lessonIndex =
    { lessonIndex
        | lessons = Dict.insert lesson.id lesson lessonIndex.lessons
        , lessonOrder = lesson.id :: lessonIndex.lessonOrder
    }


getLessonsOrdered : LessonIndex -> List Lesson
getLessonsOrdered lessonIndex =
    List.filterMap (\id -> Dict.get id lessonIndex.lessons) lessonIndex.lessonOrder
