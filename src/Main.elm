module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Article exposing (document, viewErrors)
import Browser exposing (Document)
import Browser.Events
import Element exposing (Element, centerX, centerY, clipY, column, el, fill, fillPortion, focusStyle, height, layoutWith, mouseOver, none, paddingEach, paddingXY, pointer, px, row, scrollbarY, shrink, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Lazy exposing (lazy)
import Http exposing (Error(..))
import Loader exposing (defaultConfig)
import Mark
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.Color exposing (backgroundColor, borderColor, lightPanelColor, panelColor, textColor)
import Resources.Font exposing (baseFont, bodyFontSize, giantFontSize, headingFontSize, smallFontSize, subheadingFontSize)
import Resources.Scrollbar as Scrollbar
import Time


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Article =
    { title : String
    }


type alias Model =
    { articles : List Article
    , article : Timeline (WebData String)
    , windowSize : WindowSize
    }


type Msg
    = ArticleReceived (WebData String)
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
                , expect = Http.expectString (RemoteData.fromResult >> ArticleReceived)
                }
            )


headerView : Element Msg
headerView =
    row
        [ width fill
        , height (px 75)
        , Border.color borderColor
        , Border.widthEach
            { allSidesZero
                | bottom = 1
            }
        ]
        []


articleListView : List Article -> Element Msg
articleListView =
    lazy
        (\articles ->
            column
                [ width fill
                , height fill
                , Background.color panelColor
                , Border.color borderColor
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
                            , Border.color borderColor
                            , Border.width 1
                            , paddingXY 20 10
                            , Border.rounded 5
                            , pointer
                            , mouseOver [ Background.color lightPanelColor ]
                            , onClick (LoadArticle article.title)
                            ]
                            (text article.title)
                    )
                    articles
                )
        )


articleView : String -> Element msg
articleView =
    lazy
        (\source ->
            textColumn
                [ width fill
                , height fill
                ]
                (case Mark.compile document source of
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


remoteDataView : Timeline (WebData String) -> Element msg
remoteDataView =
    lazy
        (\article ->
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
                        (Loader.view
                            { defaultConfig
                                | color = panelColor
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

                RemoteData.Success source ->
                    articleView source
        )


articleReaderView : Timeline (WebData String) -> Element msg
articleReaderView article =
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
                (remoteDataView article)
            , el [ width fill ] none
            ]
        ]


bodyView : Model -> Element Msg
bodyView model =
    row
        [ width fill
        , height fill
        , clipY
        ]
        [ articleListView model.articles
        , articleReaderView model.article
        ]


rootView : Model -> Element Msg
rootView model =
    column
        [ width fill
        , height (px model.windowSize.height)
        , Background.color backgroundColor
        ]
        [ headerView
        , bodyView model
        ]


view : Model -> Document Msg
view model =
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
            , Font.color textColor
            , Scrollbar.color borderColor
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
