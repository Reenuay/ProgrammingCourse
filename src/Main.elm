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
import Http
import Loader exposing (defaultConfig)
import Mark
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Resources.Color exposing (backgroundColor, borderColor, lightPanelColor, panelColor, textColor)
import Resources.Font exposing (baseFont, bodyFontSize, headingFontSize, smallFontSize, subheadingFontSize)
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
    , source : WebData String
    , timeline : Timeline ()
    , windowSize : WindowSize
    }


type Msg
    = ArticleReceived (WebData String)
    | FrameReceived Time.Posix
    | WindowResized Int Int
    | LoadArticle String


zeroSides : { top : number, bottom : number, left : number, right : number }
zeroSides =
    { top = 0, bottom = 0, left = 0, right = 0 }


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .timeline
            (\newTimeline model -> { model | timeline = newTimeline })


articles : List String
articles =
    [ "Example"
    , "Another example"
    , "Yet another example"
    ]


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { articles = List.map (\title -> { title = title }) articles
      , source = RemoteData.NotAsked
      , timeline = Animator.init ()
      , windowSize = windowSize
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArticleReceived result ->
            ( { model
                | source = result
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
            ( { model | source = RemoteData.Loading }
            , Http.get
                { url = "articles/" ++ title ++ ".emu"
                , expect = Http.expectString (RemoteData.fromResult >> ArticleReceived)
                }
            )


layout : Model -> Element Msg
layout model =
    -- Root
    column
        [ width fill
        , height (px model.windowSize.height)
        , Background.color backgroundColor
        ]
        [ -- Header
          row
            [ width fill
            , height (px 75)
            , Border.color borderColor
            , Border.widthEach
                { zeroSides
                    | bottom = 1
                }
            ]
            []
        , --Body
          row
            [ width fill
            , height fill
            , clipY
            ]
            [ -- Left panel
              column
                [ width fill
                , height fill
                , Background.color panelColor
                , Border.color borderColor
                , Border.widthEach { zeroSides | right = 1 }
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
                    model.articles
                )
            , -- Article
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
                        (case model.source of
                            RemoteData.NotAsked ->
                                el [ Font.size subheadingFontSize, centerX, centerY ] (text "No article is open")

                            RemoteData.Loading ->
                                el [ centerX, centerY ]
                                    (Loader.view
                                        { defaultConfig
                                            | color = panelColor
                                        }
                                        model.timeline
                                    )

                            RemoteData.Failure _ ->
                                el [ centerX, centerY ] (text "Error!")

                            RemoteData.Success source ->
                                textColumn
                                    [ width fill
                                    , height fill
                                    ]
                                <|
                                    case Mark.compile document source of
                                        Mark.Success article ->
                                            el
                                                [ Font.size headingFontSize
                                                , paddingEach { zeroSides | bottom = 30 }
                                                , Font.semiBold
                                                ]
                                                (text article.metadata.title)
                                                :: article.body
                                                ++ [ el [ height (px 60) ] none ]

                                        Mark.Almost { result, errors } ->
                                            result.body ++ viewErrors errors

                                        Mark.Failure errors ->
                                            viewErrors errors
                        )
                    , el [ width fill ] none
                    ]
                ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "Test Elm Markup"
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
            (layout model)
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
