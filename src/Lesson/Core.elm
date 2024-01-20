module Lesson.Core exposing (Block(..), Frontmatter, Lesson, LessonCompilationOutcome, StyledText, Styles)

import Mark exposing (Outcome, Partial)
import Mark.Error exposing (Error)


type alias Frontmatter =
    { title : String, author : String }


type alias Styles =
    { bold : Bool, italic : Bool, strike : Bool }


type alias StyledText =
    { styles : Styles, content : String }


type Block
    = Paragraph (List StyledText)


type alias Lesson =
    { frontmatter : Frontmatter, body : List Block }


type alias LessonCompilationOutcome =
    Outcome (List Error) (Partial Lesson) Lesson
