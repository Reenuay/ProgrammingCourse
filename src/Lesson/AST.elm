module Lesson.AST exposing (Block(..), Lesson, LessonCompilationOutcome, Metadata, StyledText, Styles)

import Mark exposing (Outcome, Partial)
import Mark.Error exposing (Error)


type alias Metadata =
    { title : String, author : String }


type alias Styles =
    { bold : Bool, italic : Bool, strike : Bool }


type alias StyledText =
    { styles : Styles, content : String }


type Block
    = Paragraph (List StyledText)


type alias Lesson =
    { metadata : Metadata, body : List Block }


type alias LessonCompilationOutcome =
    Outcome (List Error) (Partial Lesson) Lesson
