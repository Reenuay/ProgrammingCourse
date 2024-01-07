module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Article exposing (CompiledArticle, document, viewErrors)
import Browser exposing (Document)
import Browser.Events
import Common.Color
import Components.Loader as Loader
import Components.ThemeToggle as ThemeSwitch
import Element exposing (Element, alignRight, centerX, centerY, clipY, column, el, fill, fillPortion, focusStyle, fromRgb, height, layoutWith, mouseOver, none, padding, paddingEach, paddingXY, pointer, px, row, scrollbarY, shrink, spacing, text, textColumn, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Lazy exposing (lazy, lazy2)
import Http exposing (Error(..))
import Mark exposing (Outcome, Partial)
import Mark.Error exposing (Error)
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.FontSize exposing (baseFont, bodyFontSize, giantFontSize, headingFontSize, smallFontSize, subheadingFontSize)
import Resources.Style as Style
import Resources.Theme exposing (Theme, ThemeName(..), getTheme, toggleTheme)
import Time


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Article =
    { title : String
    }


type alias ArticleCompilationOutcome msg =
    Outcome (List Error) (Partial (CompiledArticle msg)) (CompiledArticle msg)


type alias Model =
    { articles : List Article
    , article : Timeline (WebData (ArticleCompilationOutcome Msg))
    , windowSize : WindowSize
    , themeName : Timeline ThemeName
    }


type Msg
    = ArticleReceived (WebData (ArticleCompilationOutcome Msg))
    | FrameReceived Time.Posix
    | WindowResized Int Int
    | LoadArticle String
    | ToggleTheme


allSidesZero : { top : number, bottom : number, left : number, right : number }
allSidesZero =
    { top = 0, bottom = 0, left = 0, right = 0 }


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .article
            (\newTimeline model -> { model | article = newTimeline })
            (\article -> RemoteData.isLoading article)
        |> Animator.watching
            .themeName
            (\newTimeline model -> { model | themeName = newTimeline })


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { articles =
            List.map (\title -> { title = title })
                [ "Example"
                , "Another example"
                , "Yet another example"
                ]
      , article = Animator.init RemoteData.NotAsked
      , windowSize = windowSize
      , themeName = Animator.init Dark
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArticleReceived source ->
            ( { model
                | article =
                    Animator.go Animator.immediately source model.article
              }
            , Cmd.none
            )

        FrameReceived time ->
            ( Animator.update time animator model
            , Cmd.none
            )

        WindowResized width height ->
            ( { model
                | windowSize = { width = width, height = height }
              }
            , Cmd.none
            )

        LoadArticle title ->
            ( { model
                | article =
                    Animator.go Animator.immediately RemoteData.Loading model.article
              }
            , Http.get
                { url = "articles/" ++ title ++ ".emu"
                , expect =
                    Http.expectString
                        (RemoteData.fromResult
                            >> RemoteData.map (Mark.compile document)
                            >> ArticleReceived
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
                    Animator.go Animator.quickly
                        (toggleTheme currentThemeName)
                        model.themeName
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


articleListView : Theme -> List Article -> Element Msg
articleListView =
    lazy2
        (\theme articles ->
            column
                [ width fill
                , height fill
                , Background.color theme.panelColor
                , Border.color theme.borderColor
                , Border.widthEach { allSidesZero | right = 1 }
                , paddingXY 10 10
                , spacing 10
                ]
                (List.map
                    (\article ->
                        el
                            [ Font.size smallFontSize
                            , width fill
                            , height shrink
                            , Border.color theme.borderColor
                            , Border.width 1
                            , paddingXY 20 10
                            , Border.rounded 5
                            , pointer
                            , mouseOver [ Background.color theme.panelHighlightColor ]
                            , onClick (LoadArticle article.title)
                            ]
                            (text article.title)
                    )
                    articles
                )
        )


articleView : ArticleCompilationOutcome msg -> Element msg
articleView =
    lazy
        (\articleOutcome ->
            textColumn
                [ width fill
                , height fill
                ]
                (case articleOutcome of
                    Mark.Success article ->
                        let
                            title =
                                [ el
                                    [ Font.size headingFontSize
                                    , paddingEach { allSidesZero | bottom = 30 }
                                    , Font.semiBold
                                    ]
                                    (text article.metadata.title)
                                ]

                            footer =
                                [ el [ height (px 60) ] none ]
                        in
                        List.concat
                            [ title
                            , article.body
                            , footer
                            ]

                    Mark.Almost { result, errors } ->
                        result.body ++ viewErrors errors

                    Mark.Failure errors ->
                        viewErrors errors
                )
        )


articleLoaderView : Theme -> Timeline (WebData (ArticleCompilationOutcome msg)) -> Element msg
articleLoaderView =
    let
        config =
            Loader.defaultConfig
    in
    lazy2
        (\theme article ->
            case Animator.current article of
                RemoteData.NotAsked ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "No article is open")

                RemoteData.Loading ->
                    el [ centerX, centerY ]
                        (Loader.bars
                            { config
                                | color = theme.panelColor
                            }
                            article
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
                            (text "Article not found")
                        ]

                RemoteData.Failure _ ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        , Style.style [ Style.unselectable ]
                        ]
                        (text "Error loading article")

                RemoteData.Success outcome ->
                    articleView outcome
        )


articleContainerView : Theme -> Timeline (WebData (ArticleCompilationOutcome msg)) -> Element msg
articleContainerView theme article =
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
                (articleLoaderView theme article)
            , el [ width (fillPortion 2) ] none
            ]
        ]


bodyView : Theme -> List Article -> Timeline (WebData (ArticleCompilationOutcome Msg)) -> Element Msg
bodyView theme articles article =
    row
        [ width fill
        , height fill
        , clipY
        ]
        [ articleListView theme articles
        , articleContainerView theme article
        ]


mergeThemes : Timeline ThemeName -> Theme
mergeThemes themeName =
    let
        isInTransition =
            Animator.current themeName /= Animator.arrived themeName
    in
    if isInTransition then
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
            [ Font.size bodyFontSize
            , Font.family baseFont
            , Font.color theme.textColor
            , Style.style
                [ Style.scrollbarThumbColor theme.borderColor
                , Style.textSelectionColor textSelectionColor
                ]
            ]
            (column
                [ width fill
                , height (px model.windowSize.height)
                , Background.color theme.backgroundColor
                ]
                [ headerView theme model.themeName
                , bodyView theme model.articles model.article
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
