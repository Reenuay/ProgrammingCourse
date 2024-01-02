module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Article exposing (document, viewErrors)
import Browser exposing (Document)
import Browser.Events
import Element exposing (Element, clip, clipY, column, fill, fillPortion, focusStyle, height, layoutWith, noHover, paddingXY, px, row, scrollbarY, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http
import Loader exposing (defaultConfig)
import Mark
import Platform.Cmd as Cmd
import Remote
import RemoteData exposing (RemoteData(..), WebData)
import Resources.Color exposing (backgroundColor, borderColor, panelColor, textColor)
import Resources.Font exposing (baseFont, bodyFontSize)
import Time


type alias WindowSize =
    { width : Int, height : Int }


type alias Model =
    { source : WebData String
    , timeline : Timeline ()
    , windowSize : WindowSize
    }


type Msg
    = GotArticle (WebData String)
    | AnimationTick Time.Posix
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
        , expect = Http.expectString (RemoteData.fromResult >> GotArticle)
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotArticle result ->
            ( { model
                | source = result
              }
            , Cmd.none
            )

        AnimationTick time ->
            ( Animator.update time animator model
            , Cmd.none
            )

        WindowResized width height ->
            ( { model
                | windowSize = { width = width, height = height }
              }
            , Cmd.none
            )


layout : Model -> Element msg
layout model =
    let
        articleView source =
            case Mark.compile document source of
                Mark.Success article ->
                    article

                Mark.Almost { result, errors } ->
                    result ++ viewErrors errors

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
                ]
                [ text "Hi!" ]
            , -- Article
              textColumn
                [ width (fillPortion 4)
                , height fill
                , paddingXY 200 30
                , spacing 20
                , scrollbarY
                ]
                (Remote.view
                    (Loader.view { defaultConfig | color = backgroundColor } model.timeline)
                    articleView
                    model.source
                )
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
            [ Font.size bodyFontSize, Font.family baseFont, Font.color textColor ]
            (layout model)
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animator.toSubscription AnimationTick model animator
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
