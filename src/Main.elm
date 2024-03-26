module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Browser exposing (Document)
import Browser.Events
import Common.Color
import Common.Util exposing (allSidesZero)
import Components.Loader as Loader
import Components.ThemeToggle as ThemeToggle
import Element exposing (Element, alignRight, centerX, centerY, clipY, column, el, fill, fillPortion, focusStyle, fromRgb, height, html, layoutWith, none, padding, paddingXY, row, scrollbarY, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy exposing (lazy, lazy2)
import Html
import Http exposing (Error(..))
import Lesson.Core exposing (Lesson, LessonCompilationOutcome)
import Lesson.Index.Core exposing (LessonIndex)
import Lesson.Index.Decoder
import Lesson.Parser
import Lesson.Renderer
import Mark
import Material.Icons.Outlined
import Material.Icons.Types exposing (Coloring(..))
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.FontSize exposing (baseFont, bodyFontSize, giantFontSize, subheadingFontSize)
import Resources.Style as Style
import Resources.Theme as Theme exposing (ColorScheme, Theme(..))
import Time


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { lessonIndex : WebData LessonIndex
    , openLesson : Timeline (WebData (Maybe Lesson))
    , windowSize : WindowSize
    , theme : Timeline Theme
    }


type Command
    = LoadLesson String
    | ToggleTheme


type SubscriptionEvent
    = FrameReceived Time.Posix
    | WindowResized Int Int


type Event
    = LessonIndexReceived (WebData LessonIndex)
    | LessonReceived (WebData LessonCompilationOutcome)
    | SubscriptionEventReceived SubscriptionEvent


type Message
    = Command Command
    | Event Event


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .openLesson
            (\newTimeline model -> { model | openLesson = newTimeline })
            (\lesson -> RemoteData.isLoading lesson)
        |> Animator.watching
            .theme
            (\newTimeline model -> { model | theme = newTimeline })


initialize : WindowSize -> ( Model, Cmd Event )
initialize windowSize =
    ( { lessonIndex = RemoteData.Loading
      , openLesson = Animator.init RemoteData.NotAsked
      , windowSize = windowSize
      , theme = Animator.init Dark
      }
    , Http.get
        { url = "lessonIndex.json"
        , expect =
            Lesson.Index.Decoder.lessonIndexDecoder
                |> Http.expectJson (RemoteData.fromResult >> LessonIndexReceived)
        }
    )


handleCommand : Command -> Model -> ( Model, Cmd Event )
handleCommand command model =
    case command of
        LoadLesson fileName ->
            ( { model
                | openLesson =
                    Animator.go Animator.immediately RemoteData.Loading model.openLesson
              }
            , Http.get
                { url = "lessons/" ++ fileName ++ ".emu"
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
                    Animator.current model.theme
            in
            ( { model
                | theme =
                    Animator.go Animator.quickly (Theme.toggle currentThemeName) model.theme
              }
            , Cmd.none
            )


handleSubscriptionEvent : SubscriptionEvent -> Model -> Model
handleSubscriptionEvent event model =
    case event of
        FrameReceived time ->
            Animator.update time animator model

        WindowResized width height ->
            { model | windowSize = { width = width, height = height } }


handleEvent : Event -> Model -> Model
handleEvent event model =
    case event of
        LessonIndexReceived source ->
            { model | lessonIndex = source }

        LessonReceived compiledLesson ->
            let
                openLesson =
                    compiledLesson
                        |> RemoteData.map
                            (\outcome ->
                                case outcome of
                                    Mark.Success lesson ->
                                        Just lesson

                                    Mark.Almost { result } ->
                                        Just result

                                    Mark.Failure _ ->
                                        Nothing
                            )
            in
            { model
                | openLesson =
                    Animator.go Animator.immediately openLesson model.openLesson
            }

        SubscriptionEventReceived ev ->
            handleSubscriptionEvent ev model


handleMessage : Message -> Model -> ( Model, Cmd Event )
handleMessage message model =
    case message of
        Command command ->
            handleCommand command model

        Event event ->
            ( handleEvent event model, Cmd.none )


renderHeader : ColorScheme -> Timeline Theme -> Element Command
renderHeader colorScheme theme =
    row
        [ width fill
        , Border.color colorScheme.borderColor
        , Border.widthEach
            { allSidesZero
                | bottom = 1
            }
        , padding 20
        ]
        [ el [ alignRight, centerY ]
            (ThemeToggle.render 50 colorScheme.panelColor colorScheme.textColor ToggleTheme theme)
        ]


renderLesson : ColorScheme -> Timeline (WebData (Maybe Lesson)) -> Element msg
renderLesson =
    lazy2
        (\theme lessonTimeline ->
            case Animator.current lessonTimeline of
                RemoteData.NotAsked ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "No lesson is opened")

                RemoteData.Loading ->
                    el [ centerX, centerY ]
                        (Loader.render 80 theme.panelColor lessonTimeline)

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

                RemoteData.Success (Just lesson) ->
                    lazy Lesson.Renderer.render lesson

                RemoteData.Success Nothing ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "Lesson file is corrupted")
        )


renderLessonContainer : ColorScheme -> Timeline (WebData (Maybe Lesson)) -> Element msg
renderLessonContainer theme lessonTimeline =
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
                (renderLesson theme lessonTimeline)
            , el [ width (fillPortion 2) ] none
            ]
        ]


renderBody : ColorScheme -> WebData LessonIndex -> Timeline (WebData (Maybe Lesson)) -> Element msg
renderBody theme lessonIndex lessonTimeline =
    let
        loader =
            el [ centerX, centerY ]
                (Loader.render 80 theme.panelColor lessonTimeline)
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
                [ renderLessonContainer theme lessonTimeline
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


renderDocument : Model -> Document Command
renderDocument model =
    let
        theme =
            Theme.getCurrentColorScheme model.theme

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
                [ renderHeader theme model.theme
                , renderBody theme model.lessonIndex model.openLesson
                ]
            )
        ]
    }


subscribe : Model -> Sub SubscriptionEvent
subscribe model =
    Sub.batch
        [ Animator.toSubscription FrameReceived model animator
        , Browser.Events.onResize WindowResized
        ]


main : Program WindowSize Model Message
main =
    let
        init flags =
            initialize flags
                |> Tuple.mapSecond (Cmd.map Event)

        update message model =
            handleMessage message model
                |> Tuple.mapSecond (Cmd.map Event)

        view model =
            let
                doc =
                    renderDocument model
            in
            { title = doc.title
            , body =
                doc.body
                    |> List.map (Html.map Command)
            }

        subscriptions model =
            subscribe model
                |> Sub.map SubscriptionEventReceived
                |> Sub.map Event
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
