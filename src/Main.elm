module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Browser exposing (Document)
import Browser.Events
import Common.Color
import Common.Util exposing (allSidesZero)
import Components.Loader as Loader
import Components.ThemeToggle as ThemeSwitch
import Element exposing (Element, alignRight, centerX, centerY, clipY, column, el, fill, fillPortion, focusStyle, fromRgb, height, html, layoutWith, mouseOver, none, padding, paddingXY, pointer, row, scrollbarY, shrink, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Lazy exposing (lazy, lazy2)
import Http exposing (Error(..))
import Lesson.Core exposing (LessonCompilationOutcome)
import Lesson.Index.Core exposing (Index, Lesson)
import Lesson.Index.Decoder
import Lesson.Parser
import Lesson.Renderer
import Mark
import Material.Icons.Outlined
import Material.Icons.Types exposing (Coloring(..))
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.FontSize exposing (baseFont, bodyFontSize, giantFontSize, smallFontSize, subheadingFontSize)
import Resources.Style as Style
import Resources.Theme exposing (Theme, ThemeName(..), getTheme, toggleTheme)
import Time


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { lessonIndex : WebData Index
    , openLesson : Timeline (WebData LessonCompilationOutcome)
    , windowSize : WindowSize
    , themeName : Timeline ThemeName
    }


type Msg
    = LessonIndexReceived (WebData Index)
    | LessonReceived (WebData LessonCompilationOutcome)
    | FrameReceived Time.Posix
    | WindowResized Int Int
    | LoadLesson String
    | ToggleTheme


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .openLesson
            (\newTimeline model -> { model | openLesson = newTimeline })
            (\lesson -> RemoteData.isLoading lesson)
        |> Animator.watching
            .themeName
            (\newTimeline model -> { model | themeName = newTimeline })


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { lessonIndex = RemoteData.Loading
      , openLesson = Animator.init RemoteData.NotAsked
      , windowSize = windowSize
      , themeName = Animator.init Dark
      }
    , Http.get
        { url = "lessonIndex.json"
        , expect =
            Http.expectJson (RemoteData.fromResult >> LessonIndexReceived)
                Lesson.Index.Decoder.lessonIndexDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LessonIndexReceived source ->
            ( { model | lessonIndex = source }
            , Cmd.none
            )

        LessonReceived source ->
            ( { model
                | openLesson =
                    Animator.go Animator.immediately source model.openLesson
              }
            , Cmd.none
            )

        FrameReceived time ->
            ( Animator.update time animator model
            , Cmd.none
            )

        WindowResized width height ->
            ( { model | windowSize = { width = width, height = height } }
            , Cmd.none
            )

        LoadLesson title ->
            ( { model
                | openLesson =
                    Animator.go Animator.immediately RemoteData.Loading model.openLesson
              }
            , Http.get
                { url = "lessons/" ++ title ++ ".emu"
                , expect =
                    Http.expectString
                        (RemoteData.fromResult
                            >> RemoteData.map (Mark.compile Lesson.Parser.lesson)
                            >> LessonReceived
                        )
                }
            )

        ToggleTheme ->
            let
                currentThemeName =
                    Animator.current model.themeName
            in
            ( { model
                | themeName =
                    Animator.go Animator.quickly (toggleTheme currentThemeName) model.themeName
              }
            , Cmd.none
            )


headerView : Theme -> Timeline ThemeName -> Element Msg
headerView theme themeName =
    let
        config =
            ThemeSwitch.defaultConfig
    in
    row
        [ width fill
        , Border.color theme.borderColor
        , Border.widthEach
            { allSidesZero
                | bottom = 1
            }
        , padding 20
        ]
        [ el [ alignRight, centerY ]
            (ThemeSwitch.toggle
                { config
                    | trackColor = theme.panelColor
                    , thumbColor = theme.textColor
                }
                themeName
                ToggleTheme
            )
        ]


lessonView : LessonCompilationOutcome -> Element msg
lessonView =
    lazy Lesson.Renderer.renderOutcome


lessonLoaderView : Theme -> Timeline (WebData LessonCompilationOutcome) -> Element msg
lessonLoaderView =
    let
        config =
            Loader.defaultConfig
    in
    lazy2
        (\theme lesson ->
            case Animator.current lesson of
                RemoteData.NotAsked ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "No lesson is open")

                RemoteData.Loading ->
                    el [ centerX, centerY ]
                        (Loader.bars
                            { config
                                | color = theme.panelColor
                            }
                            lesson
                        )

                RemoteData.Failure (BadStatus 404) ->
                    column [ centerX, centerY, spacing 40 ]
                        [ el
                            [ Font.size giantFontSize
                            , centerX
                            , Style.style [ Style.unselectable ]
                            ]
                            (text "¯\\_(ツ)_/¯")
                        , el
                            [ Font.size subheadingFontSize
                            , centerX
                            , Style.style [ Style.unselectable ]
                            ]
                            (text "Lesson not found")
                        ]

                RemoteData.Failure _ ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "Error loading lesson")

                RemoteData.Success compilationOutcome ->
                    lessonView compilationOutcome
        )


lesssonContainerView : Theme -> Timeline (WebData LessonCompilationOutcome) -> Element msg
lesssonContainerView theme lesson =
    row
        [ width (fillPortion 6)
        , height fill
        , paddingXY 4 10
        ]
        [ row
            [ width fill
            , height fill
            , paddingXY 0 30
            , scrollbarY
            ]
            [ el [ width (fillPortion 2) ] none
            , el
                [ width (fillPortion 3)
                , height fill
                , spacing 20
                ]
                (lessonLoaderView theme lesson)
            , el [ width (fillPortion 2) ] none
            ]
        ]


bodyView : Theme -> WebData Index -> Timeline (WebData LessonCompilationOutcome) -> Element Msg
bodyView theme lessonIndex lesson =
    let
        config =
            Loader.defaultConfig

        loader =
            el [ centerX, centerY ]
                (Loader.bars
                    { config
                        | color = theme.panelColor
                    }
                    lesson
                )
    in
    case lessonIndex of
        RemoteData.NotAsked ->
            loader

        RemoteData.Loading ->
            loader

        RemoteData.Success _ ->
            row
                [ width fill
                , height fill
                , clipY
                ]
                [ lesssonContainerView theme lesson
                ]

        RemoteData.Failure _ ->
            column [ centerX, centerY, spacing 20 ]
                [ el [ centerX ]
                    (Material.Icons.Outlined.warning_amber 100 (Color (Common.Color.fromElementColor theme.textColor)) |> html)
                , el
                    [ Font.size subheadingFontSize
                    , centerX
                    , Style.style [ Style.unselectable ]
                    ]
                    (text "Error loading index file")
                ]


mergeThemes : Timeline ThemeName -> Theme
mergeThemes themeName =
    let
        inTransition =
            Animator.current themeName /= Animator.arrived themeName
    in
    if inTransition then
        let
            darkTheme =
                getTheme Dark

            lightTheme =
                getTheme Light

            mergeColor mapper builder =
                (Animator.color themeName <|
                    \name ->
                        case name of
                            Light ->
                                mapper lightTheme |> Common.Color.fromElementColor

                            Dark ->
                                mapper darkTheme |> Common.Color.fromElementColor
                )
                    |> Common.Color.toElementColor
                    |> builder
        in
        Theme
            |> mergeColor .backgroundColor
            |> mergeColor .panelColor
            |> mergeColor .panelHighlightColor
            |> mergeColor .borderColor
            |> mergeColor .textColor
            |> mergeColor .accentColor

    else
        Animator.current themeName
            |> getTheme


view : Model -> Document Msg
view model =
    let
        theme =
            mergeThemes model.themeName

        accentColorDeconstructed =
            toRgb theme.accentColor

        textSelectionColor =
            { accentColorDeconstructed | alpha = 0.2 }
                |> fromRgb
    in
    { title = "Programming Course"
    , body =
        [ layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Background.color theme.backgroundColor
            , Font.size bodyFontSize
            , Font.family baseFont
            , Font.color theme.textColor
            , Style.style
                [ Style.scrollbarThumbColor theme.borderColor
                , Style.textSelectionColor textSelectionColor
                ]
            ]
            (column
                [ width fill
                , height fill
                ]
                [ headerView theme model.themeName
                , bodyView theme model.lessonIndex model.openLesson
                ]
            )
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animator.toSubscription FrameReceived model animator
        , Browser.Events.onResize WindowResized
        ]


main : Program WindowSize Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
