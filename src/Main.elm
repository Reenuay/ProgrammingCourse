module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Article exposing (CompiledArticle, document, viewErrors)
import Browser exposing (Document)
import Browser.Events
import Components.Loader exposing (defaultConfig)
import Element exposing (Element, centerX, centerY, clipY, column, el, fill, fillPortion, focusStyle, height, layoutWith, mouseOver, none, paddingEach, paddingXY, pointer, px, row, scrollbarY, shrink, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Lazy exposing (lazy, lazy2)
import Http exposing (Error(..), get)
import Mark exposing (Outcome, Partial)
import Mark.Error exposing (Error)
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.Font exposing (baseFont, bodyFontSize, giantFontSize, headingFontSize, smallFontSize, subheadingFontSize)
import Resources.Scrollbar as Scrollbar
import Resources.Theme exposing (Theme, ThemeName(..), getTheme)
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
    , themeName : ThemeName
    }


type Msg
    = ArticleReceived (WebData (ArticleCompilationOutcome Msg))
    | FrameReceived Time.Posix
    | WindowResized Int Int
    | LoadArticle String


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
      , themeName = Dark
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


headerView : Theme -> Element Msg
headerView theme =
    row
        [ width fill
        , height (px 75)
        , Border.color theme.borderColor
        , Border.widthEach
            { allSidesZero
                | bottom = 1
            }
        ]
        []


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


remoteDataView : Theme -> Timeline (WebData (ArticleCompilationOutcome msg)) -> Element msg
remoteDataView =
    lazy2
        (\theme article ->
            case Animator.current article of
                RemoteData.NotAsked ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        ]
                        (text "No article is open")

                RemoteData.Loading ->
                    el [ centerX, centerY ]
                        (Components.Loader.bars
                            { defaultConfig
                                | color = theme.panelColor
                            }
                            article
                        )

                RemoteData.Failure (BadStatus 404) ->
                    column [ centerX, centerY, spacing 40 ]
                        [ el
                            [ Font.size giantFontSize
                            , centerX
                            ]
                            (text "¯\\_(ツ)_/¯")
                        , el
                            [ Font.size subheadingFontSize
                            , centerX
                            ]
                            (text "Article not found")
                        ]

                RemoteData.Failure _ ->
                    el
                        [ Font.size subheadingFontSize
                        , centerX
                        , centerY
                        ]
                        (text "Error loading article")

                RemoteData.Success outcome ->
                    articleView outcome
        )


articleReaderView : Theme -> Timeline (WebData (ArticleCompilationOutcome msg)) -> Element msg
articleReaderView theme article =
    row
        [ width (fillPortion 4)
        , height fill
        , paddingXY 4 10
        ]
        [ row
            [ width fill
            , height fill
            , paddingXY 0 30
            , scrollbarY
            ]
            [ el [ width fill ] none
            , el
                [ width (fillPortion 2)
                , height fill
                , spacing 20
                ]
                (remoteDataView theme article)
            , el [ width fill ] none
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
        , articleReaderView theme article
        ]


rootView : Model -> Element Msg
rootView model =
    let
        theme =
            getTheme model.themeName
    in
    column
        [ width fill
        , height (px model.windowSize.height)
        , Background.color theme.backgroundColor
        ]
        [ headerView theme
        , bodyView theme model.articles model.article
        ]


view : Model -> Document Msg
view model =
    let
        theme =
            getTheme model.themeName
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
            , Scrollbar.color theme.borderColor
            ]
            (rootView model)
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
