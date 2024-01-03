module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Article exposing (document, viewErrors)
import Browser exposing (Document)
import Browser.Events
import Element exposing (Element, clipY, column, el, fill, fillPortion, focusStyle, height, layoutWith, mouseDown, mouseOver, noHover, none, padding, paddingEach, paddingXY, pointer, px, row, scrollbarY, shrink, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http
import Loader exposing (defaultConfig)
import Mark
import Platform.Cmd as Cmd
import Remote
import RemoteData exposing (RemoteData(..), WebData)
import Resources.Color exposing (backgroundColor, borderColor, lightPanelColor, panelColor, textColor)
import Resources.Font exposing (baseFont, bodyFontSize, headingFontSize, smallFontSize)
import Resources.Scrollbar as Scrollbar
import Time


type alias WindowSize =
    { width : Int, height : Int }


type alias Model =
    { source : WebData String
    , timeline : Timeline ()
    , windowSize : WindowSize
    }


type Msg
    = ArticleReceived (WebData String)
    | FrameReceived Time.Posix
    | WindowResized Int Int


zeroSides : { top : number, bottom : number, left : number, right : number }
zeroSides =
    { top = 0, bottom = 0, left = 0, right = 0 }


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .timeline
            (\newTimeline model -> { model | timeline = newTimeline })


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { source = Loading, timeline = Animator.init (), windowSize = windowSize }
    , Http.get
        { url = "articles/example.emu"
        , expect = Http.expectString (RemoteData.fromResult >> ArticleReceived)
        }
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


articles : List String
articles =
    [ "Example"
    , "Another example"
    , "Yet another example"
    ]


layout : Model -> Element msg
layout model =
    let
        articleView source =
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
    in
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
            [ width fill, height fill, clipY ]
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
                    (text
                        >> el
                            [ Font.size smallFontSize
                            , width fill
                            , height shrink
                            , mouseOver [ Background.color lightPanelColor ]
                            , Border.color borderColor
                            , Border.width 1
                            , paddingXY 20 10
                            , Border.rounded 5
                            , pointer
                            ]
                    )
                    articles
                )
            , -- Article
              row [ width (fillPortion 4), height fill, paddingXY 4 10 ]
                [ row
                    [ width fill
                    , height fill
                    , paddingXY 0 30
                    , scrollbarY
                    ]
                    [ column [ width fill ] []
                    , textColumn
                        [ width (fillPortion 2)
                        , height fill
                        , spacing 20
                        ]
                        (Remote.view
                            (Loader.view { defaultConfig | color = backgroundColor } model.timeline)
                            articleView
                            model.source
                        )
                    , column [ width fill ] []
                    ]
                ]
            ]
        ]


view : Model -> Document msg
view model =
    { title = "Test Elm Markup"
    , body =
        [ layoutWith
            { options =
                [ noHover
                , focusStyle
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
